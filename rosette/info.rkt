#lang info

(define collection 'use-pkg-name)

;;; The format for `scribblings` is documented here:
;;;    http://docs.racket-lang.org/raco/setup-info.html
(define scribblings
  '(("guide/scribble/rosette-guide.scrbl"
     ;; Path to the main documentation file
     ;; Note: everyone's .scrbl files share the same namespace,
     ;;       so a name like `guide.scrbl` conflicts with The Racket Guide.
     (multi-page)
     ;; List of flags for building docs
     (experimental))))
     ;; Documentation category. On Racket 6.3+ this can be any string.

;; Runs the code in `private/install.rkt` before installing this collection.
(define pre-install-collection "private/install.rkt")
(define compile-omit-files '("private/install.rkt"
                             "lib/trace/report/node_modules"))

(define raco-commands
  '(("symprofile"
     rosette/lib/profile/raco
     "profile Rosette symbolic evaluation"
     #f)
    ("symtrace"
     rosette/lib/trace/raco
     "trace Rosette symbolic evaluation"
     #f)))
