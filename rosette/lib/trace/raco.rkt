#lang racket/base

(require racket/cmdline
         racket/match
         racket/string
         racket/path
         racket/function
         racket/list
         racket/pretty
         racket/format
         syntax/to-string
         raco/command-name
         "../util/module.rkt"
         "../util/syntax.rkt"
         "../util/streaming-server.rkt"
         "client-launcher.rkt"
         "compile.rkt"
         "tool.rkt")

(define symbolic-trace-verbose? #f)
(define symbolic-trace-pkgs-to-instrument '())
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
    "Instrument code in any language, not just `#lang rosette`"
    (symbolic-trace-rosette-only? #f)]
   #:multi
   [("-p" "--pkg") pkg
    "Instrument code in the given package"
    (set! symbolic-trace-pkgs-to-instrument
          (cons pkg symbolic-trace-pkgs-to-instrument))]

   ;; SymTrace options
   #:once-each
   [("--assert")
    "Skip assertion errors (not reliable)"
    (symbolic-trace-skip-assertion? #t)]
   [("--solver")
    "Skip infeasible errors with help from the solver"
    (symbolic-trace-skip-infeasible-solver? #t)]

   [("--verbose")
    "Verbose output (log the output in the JSON format to stdout)"
    (set! symbolic-trace-verbose? #t)]

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
(current-load/use-compiled (make-rosette-load/use-compiled
                            symbolic-trace-pkgs-to-instrument))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (when/null [x v] body ...)
  (let ([x v])
    (cond
      [x body ...]
      [else 'null])))

(define cwd (path-string->string (current-directory)))

(define (normalize-path-for-rosette p)
  (define path (path-string->string p))
  (if (string-prefix? path cwd)
      (string-append "<cwd>/" (path-string->string (find-relative-path cwd path)))
      path))

(define (frame->json/tail proc path line col)
  (hash 'name (~a (or (object-name proc) proc))
        'srcloc (hash 'source (normalize-path-for-rosette path)
                      'line line
                      'column col)))

(define (first-frame->json/tail stack)
  (match stack
    ['() '()]
    [(cons #f stack) (first-frame->json/tail stack)]
    [(cons (list 'certified a b) _) #:when (eq? a b) '()]
    [(cons (list _ _ elem) _)
     (list (apply frame->json/tail elem))]))

(define (each-frame->json/tail frame)
  (match frame
    [#f #f]
    [(list 'uncertified _ _) #f]
    [(list _ elem _) (apply frame->json/tail elem)]))

(define (exn-context->json xs)
  (for/list ([x (in-list xs)] [_limit (in-range 32)])
    (hash 'name (when/null [name (car x)] (~a name))
          'srcloc (when/null [loc (cdr x)]
                    (hash 'source (normalize-path-for-rosette (srcloc-source loc))
                          'line (srcloc-line loc)
                          'column (srcloc-column loc))))))

(define (entry->json entry original-map)
  (match-define (list exn-info stx-info stack _pc) entry)
  (hash 'timestamp (current-seconds)
        'exnMsg (exn-message exn-info)
        'exnTrace (exn-context->json
                    (continuation-mark-set->context
                     (exn-continuation-marks exn-info)))
        'stxInfo (when/null [stx-info stx-info]
                    (define blaming-stx (hash-ref original-map (rest stx-info) #f))
                    (hash 'stx (if blaming-stx
                                   (syntax->string #`(#,blaming-stx))
                                   (first stx-info))
                          'srcloc (hash 'source (normalize-path-for-rosette (second stx-info))
                                        'line (third stx-info)
                                        'column (fourth stx-info))))
        'callStack
        (append (first-frame->json/tail stack)
                (filter-map each-frame->json/tail stack))))

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
    (when symbolic-trace-verbose?
      (pretty-write out))
    out)
   2.0
   (thunk (hash 'type "shutdown" 'data 'null))))

(define browser-launcher
  (launch (hash 'port port
                'title (path-string->string (file-name-from-path file)))))

(match (channel-get connected-channel)
  ['connected (void)]
  [x (error "unexpected response from client" x)])

(break-thread browser-launcher)

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
