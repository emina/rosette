#lang racket/base

(require racket/cmdline
         racket/match
         racket/function
         racket/list
         racket/pretty
         racket/format
         syntax/to-string
         raco/command-name
         "../util/module.rkt"
         "../util/syntax.rkt"
         "../util/server.rkt"
         "client-launcher.rkt"
         "compile.rkt"
         "tool.rkt")

(define symbolic-trace-dev? #f)
(define module-name (make-parameter 'main))
(define file
  (command-line
   #:program (short-program+command-name)
   ; Tool configuration
   #:help-labels "" "Tracing settings"
   #:once-each

   ;; SymPro options
   [("-m" "--module") name
                      "Run submodule <name> (defaults to 'main)"
                      (module-name (string->symbol name))]
   [("-r" "--racket")
    "Instrument code in any module, not just `#lang rosette`"
    (symbolic-trace-rosette-only? #f)]

   ;; SymTrace options
   [("--assert")
    "Skip assertion errors (not reliable)"
    (symbolic-trace-skip-assertion? #t)]
   [("--solver")
    "Skip infeasible errors with help from the solver"
    (symbolic-trace-skip-infeasible-solver? #t)]

   [("--dev-verbose")
    "Also log the trace in the JSON format to stdin."
    (set! symbolic-trace-dev? #t)]


   #:help-labels ""
   #:args (filename . args)
   ; pass all unused arguments to the file being run
   (current-command-line-arguments (list->vector args))
   filename))

(collect-garbage)
(collect-garbage)
(collect-garbage)


;; module->module-path will load modules, but we do not wish to load them yet because
;; doing so will start collecting syntax information even though we have not
;; initialize variables properly yet. Therefore, make a new namespace
;; so that the loading happens in a sandbox, and so that outside of the sandbox,
;; the modules are considered not loaded.
(match-define-values (mod _)
  (parameterize ([current-namespace (make-base-namespace)])
    (module->module-path file (module-name))))

;; setup the new current-compile here so that the loading that occurs due to
;; module->module-path is relative cheap
(current-compile symbolic-trace-compile-handler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (when/null [x v] body ...)
  (let ([x v])
    (cond
      [x body ...]
      [else 'null])))

(define (frame->json frame)
  (hash 'name (~a (or (object-name (first frame)) (first frame)))
        'srcloc (hash 'source (second frame)
                      'line (third frame)
                      'column (fourth frame))))

(define (exn-context->json xs)
  (for/list ([x (in-list xs)] [_limit (in-range 32)])
    (hash 'name (when/null [name (car x)] (symbol->string name))
          'srcloc (when/null [loc (cdr x)]
                    (hash 'source (path-string->string (srcloc-source loc))
                          'line (srcloc-line loc)
                          'column (srcloc-column loc))))))

(define (entry->json entry original-map)
  (define exn-info (first entry))
  (hash 'timestamp (current-seconds)
        'exn_msg (exn-message exn-info)
        'exn_trace (exn-context->json
                    (continuation-mark-set->context
                     (exn-continuation-marks exn-info)))
        'stx_info (when/null [stx-info (second entry)]
                    (define blaming-stx (hash-ref original-map (rest stx-info) #f))
                    (hash 'stx (if blaming-stx
                                   (syntax->string #`(#,blaming-stx))
                                   (first stx-info))
                          'srcloc (hash 'source (second stx-info)
                                        'line (third stx-info)
                                        'column (fourth stx-info))))
        'call_stack (map frame->json (third entry))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Running server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here's the plan.
;; - Zeroth, spin up the worker which will handle communication between threads.
;; - First, run the socket server.
;; - Second, run the client server. This will spin up the client which
;;   will connect to the socket server.
;; - Third, now that the client is connected to the socket server, we can safely terminate
;;   the client server.
;; - Fourth, run the tracer

;; A message type is either trace, stats, or shutdown. shutdown is specially handled
(define worker
  (thread
   (thunk
    (define *queue* '())
    (let loop ()
      (match (thread-receive)
        [`(append ,e)
         (set! *queue* (cons (hash 'type "trace" 'data e) *queue*))
         (loop)]
        [`(query ,thd)
         (thread-send thd (reverse *queue*))
         (set! *queue* '())
         (loop)]
        [`(stats ,stats)
         (set! *queue* (cons (hash 'type "stats" 'data stats) *queue*))
         (loop)])))))

(define-values (port shutdown! connected-channel)
  (start-streaming-server
   (thunk
    (thread-send worker `(query ,(current-thread)))
    (define out (thread-receive))
    (when symbolic-trace-dev?
      (pretty-write out))
    out)
   2.0
   (thunk (hash 'type "shutdown" 'data 'null))))

(define browser-launcher (launch (hash 'port port 'title file)))

(match (channel-get connected-channel)
  ['connected (void)]
  [x (error "unexpected response from client" x)])


#;(break-thread browser-launcher)

(do-trace (λ () (dynamic-require mod #f))
          #:entry-handler
          (λ (entry _add-trace! original-map)
            (thread-send worker `(append ,(entry->json entry original-map))))
          #:post-proc
          (λ (current-stats _trace _original-map)
            (thread-send worker `(stats ,current-stats))))

(channel-put connected-channel 'finish)
(match (channel-get connected-channel)
  ['finish (void)]
  [x (raise x)])

(shutdown!)
(break-thread worker)

(thread-wait browser-launcher)
