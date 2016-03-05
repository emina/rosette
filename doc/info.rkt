#lang info

;; The format for `scribblings` is documented here:
;;    http://docs.racket-lang.org/raco/setup-info.html
(define scribblings
  '(("guide/scribble/rosette-guide.scrbl"
     ;; Path to the main documentation file
     ;; Note: everyone's .scrbl files share the same namespace,
     ;;       so a name like `guide.scrbl` conflicts with The Racket Guide.
     ()
     ;; List of flags for building docs
     experimental)))
     ;; Documentation category. On Racket 6.3+ this can be any string.
