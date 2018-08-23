#lang racket

(require (only-in rosette/base/core/term expression)
         "generic.rkt"
         "../../data.rkt" "../../reporter.rkt")
(provide make-terms-component)

; The terms component analyzes the program's use of symbolic terms.
; Right now, the only analysis is to see which terms never reach the solver.

(define (make-terms-component options)
  (profile-terms-component (make-hasheq) (make-hasheq) '() (mutable-seteq)))

(struct profile-terms-component (term->term-node term->creator [stack #:mutable] reached-solver) #:transparent
  #:methods gen:report-component
  [(define (init-component self)
     void)
   (define (receive-data self events)
     (get-term-analysis-messages self events))])

; a node in the term graph contains the term itself, its subterms (as nodes), and its creator
(struct term-node (term subterms creator))

; return a list of messages relating to terms -- which are unused etc
(define (get-term-analysis-messages cmpt events)
  (match-define (profile-terms-component term->term-node term->creator stack reached-solver) cmpt)

  (for ([e (in-list events)])
    (match e
      [(profile-event-enter met id loc proc in)
       (set! stack (cons id stack))]
      [(profile-event-exit met out)
       (set! stack (cdr stack))]
      [(profile-event-term-new met t)
       (define children  ; record all the term's children
         (match t
           [(expression op elts ...)
            (for/list ([e elts] #:when (hash-has-key? term->term-node e))
              (hash-ref term->term-node e))]
           [_ '()]))
       (define node (term-node t children (car stack)))
       (hash-set! term->term-node t node)
       (hash-set! term->creator t (car stack))]
      [(profile-event-solve-encode met lst)
       (for* ([assts lst][a assts] #:when (hash-has-key? term->term-node a))
         (define tn (hash-ref term->term-node a))
         (let loop ([tn tn])
           (unless (set-member? reached-solver tn)
             (set-add! reached-solver tn)
             (for ([tn* (term-node-subterms tn)]) (loop tn*)))))]
      [_ (void)]))

  ; update the call stack after processing these events
  (set-profile-terms-component-stack! cmpt stack)

  ; determine the sources of terms that did not reach the solver
  (define unused-term-sources (make-hasheq))
  (for ([(t tn) (in-hash term->term-node)] #:unless (set-member? reached-solver tn))
    (define cid (term-node-creator tn))
    (hash-set! unused-term-sources cid (add1 (hash-ref unused-term-sources cid 0))))

  ; emit that message
  (list (hash 'type "unused-terms"
              'data (for/list ([(cid n) (in-hash unused-term-sources)]) (list cid n)))))
