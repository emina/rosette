#lang s-exp syntax/module-reader
rosette
#:wrapper1 (lambda (th) 
             (parameterize 
                 ([current-readtable (make-no-vert-bar-readtable)])
               (th)))
(require no-vert-bar/reader)
