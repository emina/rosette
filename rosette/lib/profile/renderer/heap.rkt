#lang racket

(require net/sendurl
         "renderer.rkt"
         "../data.rkt" "../reporter.rkt"
         (only-in rosette/base/core/term expression))
(provide make-heap-renderer)

; The summary renderer aggregates inclusive and exclusive time
; per procedure and prints the results.

(define (make-heap-renderer source name [options (hash)])
  (heap-renderer source name))

(struct heap-renderer (source name) 
  #:transparent
  #:methods gen:renderer
  [(define start-renderer void)
   (define (finish-renderer self profile)
     (match-define (heap-renderer source name) self)
     (time (analyze-terms profile)))])


(define (analyze-terms profile)
  ; a node in the term graph contains three fields
  ; * the term
  ; * the subterms (term-node?s) the term contains
  ; * the creator of the term (a call-node?)
  (struct term-node (term subterms creator))

  ; a node in the call tree contains five fields
  ; * the name of the procedure invoked
  ; * the children invoked by the procedure (a list of call-node?s)
  ; * the total time spent in this node (including children)
  ; * a list of terms created by this call (term-node?s)
  (struct call-node (proc parent children time created-terms) #:mutable)

  ; term? -> term-node?
  (define term->node (make-hasheq))
  ; term?s that reached the solver
  (define solved-terms (mutable-seteq))
  ; current call-node?
  (define current-call #f)


  (define events (reverse (unbox (profile-state-events profile))))
  (for ([e events])
    (match e
      [(profile-event-enter met id loc proc in)
       ; temporarily store the metrics in the time field -- they'll be removed
       ; when we reach the corresponding profile-event-exit? for this call
       (define node (call-node (object-name proc) current-call '() met '()))
       (when current-call
         (set-call-node-children! current-call (cons node (call-node-children current-call))))
       (set! current-call node)]
      [(profile-event-exit met out)
       ; compute the delta metrics (the initial metrics were stored in the time field)
       (define metrics (diff-metrics (call-node-time current-call) met))
       (define time (metrics-ref metrics 'time))
       (set-call-node-time! current-call time)
       ; reverse the order of the calls since we built that list backwards
       (set-call-node-children! current-call (reverse (call-node-children current-call)))
       (unless (false? (call-node-parent current-call))
         (set! current-call (call-node-parent current-call)))]
      [(profile-event-term-new met t)
       (define children
         (match t
           [(expression op elts ...)
            (for/list ([e elts] #:when (hash-has-key? term->node e))
              (hash-ref term->node e))]
           [_ '()]))
       (define node (term-node t children current-call))
       (set-call-node-created-terms! current-call (cons node (call-node-created-terms current-call)))
       (hash-set! term->node t node)]
      [(profile-event-solve-encode met lst)
       (for* ([assts lst][a assts])
         (set-add! solved-terms a))]
      [_ void]))

  
  ; output dot
  (define p (make-temporary-file "heap~a.dot"))
  (define f (open-output-file p #:exists 'truncate))
  (fprintf f "digraph heap {\n  ordering=out;\n")

  
  ; decide which nodes will we put in the plot
  ; by chosing a time threshold based on max time spent in any one call
  (define (call-node-time-excl node)  ; exclusive time for a call-node
    (- (call-node-time node) (apply + (map call-node-time (call-node-children node)))))
  (define maxtime-excl  ; max exclusive time over all calls
    (let loop ([node current-call])
      (apply max (append (list (call-node-time-excl node)) (map loop (call-node-children node))))))
  (define maxtime-incl  ; max inclusive time over all calls
    (apply max (map call-node-time (call-node-children current-call))))
  (define threshold (* 0.001 maxtime-incl))

  ; now select the nodes to include and assign them dot names
  (define node->label (make-hasheq))
  (let loop ([node current-call])
    (when (> (call-node-time node) threshold)
      (hash-set! node->label node (format "n~v" (hash-count node->label)))
      (for ([c (call-node-children node)]) (loop c))))

  
  ; determine which terms (transitively) reached the solver
  (define reached-solver (mutable-seteq))
  (for ([t solved-terms])
    (define tn (hash-ref term->node t))
    (let loop ([tn tn])
      (unless (set-member? reached-solver tn)
        (set-add! reached-solver tn)
        (for ([tn* (term-node-subterms tn)]) (loop tn*)))))

  ; determine the sources of terms that did not reach the solver
  (define unused-term-sources (make-hasheq))
  (for ([(t tn) term->node] #:unless (set-member? reached-solver tn))
    (define src (term-node-creator tn))
    (hash-set! unused-term-sources src (add1 (hash-ref unused-term-sources src 0))))


  ; emit the definitions for each node
  (define (width dt) (exact->inexact (+ .75 (min (/ dt maxtime-excl) 1))))
  (define (height dt) (exact->inexact (+ .5 (min (/ dt maxtime-excl) 1))))
  (define (fontsize dt) (exact->inexact (+ 10 (* 8 (min (/ dt maxtime-excl) 1)))))
  (define (color node)
    (define score
      (let ([num-terms (length (call-node-created-terms node))])
        (if (or (= num-terms 0) (< num-terms (* (hash-count term->node) 0.01)))
            0
            (/ (hash-ref unused-term-sources node 0) num-terms))))
    (define 255-color (inexact->exact (round (* (- 1 score) 255))))
    (define hex-color (~r 255-color #:base 16 #:min-width 2 #:pad-string "0"))
    (format "#ff~a~a" hex-color hex-color))
  (let loop ([node current-call]) ; loop in call-graph order
    (when (hash-has-key? node->label node)
      (let* ([dt (call-node-time node)]
             [dt-excl (call-node-time-excl node)]
             [dtstr (~r dt #:precision 0)])
        (define label
          (format "~a\n~a ms\n~v/~v terms"
                  (call-node-proc node)
                  dtstr
                  (hash-ref unused-term-sources node 0)
                  (length (call-node-created-terms node))))
        (fprintf f "  ~a [shape=box,style=filled,fillcolor=\"~a\",label=\"~a\",width=~v,height=~v,fontsize=~v,fixedsize=true];\n"
                 (hash-ref node->label node)
                 (color node)
                 label
                 (width dt-excl) (height dt-excl) (fontsize dt-excl)))
      (for ([c (call-node-children node)]) (loop c))))

  
  ; put control flow edges in the plot
  (let loop ([node current-call])
    (when (hash-has-key? node->label node)
      (for ([c (call-node-children node)])
        (when (hash-has-key? node->label c)
          (fprintf f "  ~a -> ~a;\n" (hash-ref node->label node) (hash-ref node->label c))))
      (for ([c (call-node-children node)] #:when (hash-has-key? node->label c)) (loop c))))


  ; compute data flow edges
  (define dataflow (make-hasheq))
  (for ([tn (hash-values term->node)])
    (define creator (term-node-creator tn))
    (when (hash-has-key? node->label creator)
      (define from (hash-ref! dataflow creator make-hasheq))
      (for ([tn* (term-node-subterms tn)])
        (define creator* (term-node-creator tn*))
        (when (hash-has-key? node->label creator*)
          (hash-set! from creator* (add1 (hash-ref from creator* 0)))))))

  ; put data flow edges in the plot
  (define flow-counts (flatten (map hash-values (hash-values dataflow))))
  (unless (null? flow-counts)
    (define maxdataflow (apply max flow-counts))
    (define dataflow-threshold (* 0.01 maxdataflow))
    (for* ([(src dsts) dataflow]
           [(dst v) dsts]
           #:when (> v dataflow-threshold))
      (fprintf f "  ~a -> ~a [penwidth=~v,color=blue];\n"
               (hash-ref node->label src)
               (hash-ref node->label dst)
               (exact->inexact (* (/ v maxdataflow) 4)))))

  ; close the graph
  (fprintf f "}\n")
  (close-output-port f)


  (printf "sources of unused terms: ~v\n" (hash-count unused-term-sources))
  (define sorted-sources
    (sort (hash-keys unused-term-sources) > #:key (lambda (cn) (hash-ref unused-term-sources cn))))
  (define worst-sources (take sorted-sources (min 10 (length sorted-sources))))
  (for ([s worst-sources])
    (printf "* ~v - ~v\n" (call-node-proc s) (hash-ref unused-term-sources s)))
  

  ; find a place to put the pdf
  (define pdf-path (make-temporary-file "graph~a.pdf"))
  (system (format "dot -Tpdf -q -o ~a ~a" pdf-path p))
  (send-url/file pdf-path)

  (printf "Output to file: ~a\n" pdf-path))
