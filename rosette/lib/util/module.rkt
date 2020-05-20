#lang racket/base

(provide module->module-path
         make-rosette-load/use-compiled)
(require racket/string
         racket/match
         setup/dirs
         pkg/path
         custom-load)

; check if there's a module of the given name, and if not,
; import the entire file instead
(define (module->module-path file mod)
  (define file-path `(file ,file))
  (define mod-path `(submod ,file-path ,mod))
  (if (module-declared? mod-path #t)
      (values mod-path mod-path)
      (values file-path file)))

(define (path-prefix? a b)
  (string-prefix? (path->string (resolve-path a))
                  (path->string (resolve-path b))))

(define (make-rosette-load/use-compiled pkgs-to-instrument)
  (make-custom-load/use-compiled
   #:blacklist
   (λ (path)
     (cond
       [(path-prefix? path (find-collects-dir)) #f]
       [else
        (match (path->pkg path)
          [#f #t]
          [pkg-name (member pkg-name pkgs-to-instrument)])]))))
