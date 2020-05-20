#lang racket/base

(require racket/cmdline
         raco/command-name
         "../util/module.rkt"
         "compile.rkt"
         "tool.rkt"
         (only-in "record.rkt" filtering-threshold)
         "renderer/trace.rkt"
         "renderer/noop.rkt"
         "renderer/heap.rkt"
         "renderer/report.rkt")

;; raco symprofile (based on raco feature-profile)
;; profile the main submodule (if there is one), or the top-level module

(define renderer% (make-parameter make-report-renderer))
(define run-profiler? (make-parameter #t))
(define module-name (make-parameter 'main))
(define renderer-options (make-parameter (hash)))
(define pkgs-to-instrument (make-parameter '()))
(define file
  (command-line #:program (short-program+command-name)
                #:help-labels "" "Profiler modes"
                #:once-any  ; Profiler selections
                ["--trace" "Produce a complete execution trace"
                           (renderer% make-trace-renderer)]
                ["--report" "Produce an interactive report"
                          (renderer% make-report-renderer)]
                ["--stream" "Produce a streaming interactive report"
                            (renderer% make-report-stream-renderer)]
                ["--noop" "Produce no profile output (for testing)"
                          (renderer% make-noop-renderer)]
                ["--heap" "Profile a heap profile"
                          (renderer% make-heap-renderer)]
                ; Tool configuration
                #:help-labels "" "Profiled code settings"
                #:once-each
                [("-l" "--compiler-only") 
                 "Only install the compile handler; do not run the profiler"
                 (run-profiler? #f)]
                [("-m" "--module") name
                 "Run submodule <name> (defaults to 'main)"
                 (module-name (string->symbol name))]
                [("-r" "--racket")
                 "Instrument code in any language, not just `#lang rosette`"
                 (symbolic-profile-rosette-only? #f)]
                #:multi
                [("-p" "--pkg") pkg
                 "Instrument code in the given package"
                 (pkgs-to-instrument (cons pkg (pkgs-to-instrument)))]
                #:help-labels "" "Profiling settings"
                #:once-each
                [("-t" "--threshold") t
                 "Threshold (in milliseconds) for pruning cheap function calls"
                 (let ([th (string->number t)])
                   (when (or (eq? th #f) (< th 0))
                     (raise-argument-error 'threshold "number >= 0" t))
                   (filtering-threshold th))]
                ; Renderer-specific configuration
                #:help-labels "" "Mode-specific settings"
                #:once-each
                [("-d" "--delay") d
                 "Streaming report: delay between samples, in seconds"
                 (let ([de (string->number d)])
                   (when (or (eq? de #f) (<= de 0))
                     (raise-argument-error 'delay "number > 0" d))
                   (renderer-options (hash-set (renderer-options) 'interval de)))]
                [("-s" "--symlink-html")
                 "Interactive reports: symlink template instead of copying"
                 (renderer-options (hash-set (renderer-options) 'symlink #t))]
                #:help-labels ""
                #:args (filename . args)
                ; pass all unused arguments to the file being run
                (current-command-line-arguments (list->vector args))
                filename))

; Set up the renderer
(define (renderer source-stx name)
  ((renderer%) source-stx name (renderer-options)))
(current-renderer renderer)

(collect-garbage)
(collect-garbage)
(collect-garbage)

(current-compile symbolic-profile-compile-handler)
(current-load/use-compiled (make-rosette-load/use-compiled
                            (pkgs-to-instrument)))

(define-values (mod mod-pretty)
  (module->module-path file (module-name)))

(define (run)
  (dynamic-require mod #f))


(if (run-profiler?)
    (profile-thunk run #:source mod-pretty
                       #:name (format "~a" file))
    (run))
