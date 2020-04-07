#lang racket/base

(provide module->module-path)

; check if there's a module of the given name, and if not,
; import the entire file instead
(define (module->module-path file mod)
  (define file-path `(file ,file))
  (define mod-path `(submod ,file-path ,mod))
  (if (module-declared? mod-path #t)
      (values mod-path mod-path)
      (values file-path file)))
