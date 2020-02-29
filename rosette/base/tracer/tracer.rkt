#lang racket

(provide with-trace with-trace* with-subtrace query-root)
(require syntax/parse/define)

;; macros dealing with current-enable-tracer?

(define-for-syntax current-enable-tracer? (make-parameter #t))

(define-syntax-parser with-trace
  [(_ orig body ...+)
   (cond
     [(current-enable-tracer?) #'(call-with-trace #'orig (thunk body ...))]
     [else #'(begin body ...)])])

(define-simple-macro (with-trace* orig body ...+)
  (with-trace orig (with-subtrace #f body ...)))

(define-syntax-parser with-subtrace
  [(_ key body ...+)
   (cond
     [(current-enable-tracer?) #'(call-with-subtrace key (thunk body ...))]
     [else #'(begin body ...)])])

(define-syntax-parser define+provide
  [(_ (name . args) body ...)
   (cond
     [(current-enable-tracer?)
      #'(begin
          (provide name)
          (define (name . args) body ...))]
     [else #'(begin
               (provide name)
               (define-simple-macro (name . args) (begin)))])])

;; actual functionality

(struct node (stx children) #:transparent #:mutable)
(struct subnode (key children errors) #:transparent #:mutable)

(define the-root (node 'root (list (subnode 'subroot empty empty))))
(define current-subnode (match-let ([(node _ (list target)) the-root]) target))
(define current-node the-root)
(define in-subnode? #f)

(define+provide (push-error! e)
  (unless in-subnode?
    (eprintf "error ~e occurs while not in a subnode\n" e)
    (eprintf "current node: ~a\n" current-node)
    (eprintf "current subnode: ~a\n" current-subnode))
  (set-subnode-errors! current-subnode (cons e (subnode-errors current-subnode))))

(define+provide (call-with-trace orig the-thunk)
  (define new-node (node orig empty))
  (define the-node current-node)
  (set-subnode-children! current-subnode
                         (cons new-node (subnode-children current-subnode)))
  (set! current-node new-node)
  (begin0 (the-thunk)
    (set-node-children! new-node (reverse (node-children new-node)))
    (set! current-node the-node)))

(define+provide (call-with-subtrace key the-thunk)
  (define new-subnode (subnode key empty empty))
  (define the-subnode current-subnode)
  (set-node-children! current-node
                      (cons new-subnode (node-children current-node)))
  (set! current-subnode new-subnode)
  (set! in-subnode? #t)
  (begin0 (the-thunk)
    (set! in-subnode? #f)
    (set-subnode-children! new-subnode (reverse (subnode-children new-subnode)))
    (set-subnode-errors! new-subnode (reverse (subnode-errors new-subnode)))
    (set! current-subnode the-subnode)))

(define (query-root)
  ;; root doesn't have a finalizer, so do it manually
  (match-define (node _ (list (subnode _ children _))) the-root)
  (reverse children))
