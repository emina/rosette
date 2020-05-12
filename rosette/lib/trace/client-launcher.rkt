#lang racket/base

(provide launch)
(require racket/runtime-path
         racket/function
         racket/string
         racket/format
         net/uri-codec
         web-server/servlet-env
         web-server/dispatchers/dispatch)

(define-runtime-path here ".")

(define (launch kv)
  (thread
   (thunk
    (serve/servlet
     (thunk* (next-dispatcher))
     #:servlet-path
     (string-append
      "/?"
      (string-join
       (for/list ([(k v) (in-hash kv)])
         (format "~a=~a" (~a k) (uri-encode (~a v))))
       "&"))
     #:extra-files-paths
     (list (build-path here "report" "dist"))))))
