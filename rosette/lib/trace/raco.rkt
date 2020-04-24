#lang racket/base

(require racket/cmdline
         racket/match
         racket/string
         racket/list
         racket/exn
         syntax/to-string
         raco/command-name
         "../util/module.rkt"
         "compile.rkt"
         "tool.rkt")

(define symbolic-trace-streaming? (make-parameter #f))
(define symbolic-trace-show-stats? (make-parameter #f))
(define symbolic-trace-context-length (make-parameter 3))

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
   [("--stream")
    "Stream report instead of reporting the result at the end"
    (symbolic-trace-streaming? #t)]
   [("--assert")
    "Skip assertion errors (not reliable)"
    (symbolic-trace-skip-assertion? #t)]
   [("--solver")
    "Skip infeasible errors with help from the solver"
    (symbolic-trace-skip-infeasible-solver? #t)]
   [("--context") context-length
    "Set the length of the exception context"
    (symbolic-trace-context-length (string->number context-length))]
   [("--stats")
    "Show statistics"
    (symbolic-trace-show-stats? #t)]


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

(define (print-element e original-map)
  (define msg (parameterize ([error-print-context-length (symbolic-trace-context-length)])
                (exn->string (first e))))
  (define at
    (cond
      [(second e)
       (define blaming-stx (hash-ref original-map (rest (second e)) #f))
       (format "~a line ~a column ~a\n~a\n"
               (second (second e))
               (third (second e))
               (fourth (second e))
               (if blaming-stx
                   (syntax->string #`(#,blaming-stx))
                   (first (second e))))]
      [else #f]))
  (define call-stack (if (third e) (map frame->string (third e)) '()))
  (displayln (make-string 80 #\-) (current-error-port))
  (print-table "exn" msg
               "at" at
               "call stack" (if (empty? call-stack)
                                ""
                                (string-join call-stack "\n" #:before-first "\n")))
  (newline (current-error-port)))

(define (frame->string frame)
  (format "~a:~a:~a ~a"
          (second frame)
          (third frame)
          (fourth frame)
          (or (object-name (first frame)) (first frame))))

(define print-table
  (match-lambda*
    [(list) (void)]
    [(list k v xs ...)
     (eprintf "~a: ~a\n" k v)
     (apply print-table xs)]))

(do-trace (λ () (dynamic-require mod #f))
          #:entry-handler
          (λ (entry add-trace! original-map)
            (cond
              [(symbolic-trace-streaming?)
               (print-element entry original-map)]
              [else (add-trace! entry)]))
          #:post-proc
          (λ (current-stats trace original-map)
            (cond
              [(symbolic-trace-streaming?) (void)]
              [else (for-each (λ (e) (print-element e original-map)) trace)])

            (when (symbolic-trace-show-stats?)
              (eprintf "--- Stats ---------------------------\n")
              (for ([(k v) (in-hash current-stats)])
                (eprintf "~a: ~a\n" k v)))))
