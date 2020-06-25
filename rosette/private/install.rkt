#lang racket/base

;; Check whether z3 is installed during package setup.
;; If missing, builds & links a z3 binary.


(provide pre-installer)

(require racket/match
         racket/file
         racket/port
         net/url
         file/unzip)

(define (print-failure path msg)
  (printf "\n\n********** Failed to install Z3 **********\n\n")
  (printf "You'll need to manually install a Z3 binary\n")
  (printf "to this location: ~a\n\n" path)
  (printf "The problem was:\n~a\n\n" msg)
  (printf "*********\n\n\n"))

(define (pre-installer collections-top-path racl-path)
  (define bin-path (simplify-path (build-path racl-path ".." "bin")))
  (define z3-path (build-path bin-path "z3"))
  (with-handlers ([exn:fail? (lambda (e) (print-failure z3-path (exn-message e)))])
    (unless (custom-z3-symlink? z3-path)
      (define-values (z3-url z3-path-in-zip) (get-z3-url))
      (define z3-port (get-pure-port (string->url z3-url) #:redirections 10))  
      (make-directory* bin-path) ;; Ensure that `bin-path` exists
      (delete-directory/files z3-path #:must-exist? #f) ;; Delete old version of Z3, if any
      (parameterize ([current-directory bin-path])    
        (call-with-unzip z3-port
                         (λ (dir)
                           (copy-directory/files (build-path dir z3-path-in-zip) z3-path)))
        ;; Unzipping loses file permissions, so we reset the z3 binary here
        (file-or-directory-permissions 
          z3-path 
          (if (equal? (system-type) 'windows) #o777 #o755))))))

(define (custom-z3-symlink? z3-path)
  (and (file-exists? z3-path)
       (let ([p (simplify-path z3-path)])
         (not (equal? (resolve-path p) p)))))

(define (get-z3-url)
  (define site "https://github.com/Z3Prover/z3/releases/download")
  (define version "z3-4.8.8")
  (define-values (os exe)
    (match (list (system-type) (system-type 'word))
      ['(unix 64)    (values "x64-ubuntu-16.04" "z3")]
      [`(macosx ,_)  (values "x64-osx-10.14.6" "z3")]
      ['(windows 64) (values "x64-win" "z3.exe")]
      [any           (raise-user-error 'get-z3-url "Unknown system type '~a'" any)]))
  (define name (format "~a-~a" version os))
  (values
   (format "~a/~a/~a.zip" site version name)
   (format "~a/bin/~a" name exe)))


;; A copy of net/url's get-pure-port/headers, except with the Location header
;; for redirects made case-insensitive, fixing https://github.com/racket/racket/pull/3057
(require net/http-client net/url-connect)
(define (get-pure-port url #:redirections [redirections 0])
  (let redirection-loop ([redirections redirections] [url url])
    (define hc (http-conn-open (url-host url)
                               #:ssl? (if (equal? "https" (url-scheme url))
                                          (current-https-protocol)
                                          #f)))
    (define access-string
      (url->string
       ;; RFCs 1945 and 2616 say:
       ;;   Note that the absolute path cannot be empty; if none is present in
       ;;   the original URI, it must be given as "/" (the server root).
       (let-values ([(abs? path)
                     (if (null? (url-path url))
                         (values #t (list (make-path/param "" '())))
                         (values (url-path-absolute? url) (url-path url)))])
         (make-url #f #f #f #f abs? path (url-query url) (url-fragment url)))))
    (http-conn-send! hc access-string #:method #"GET" #:content-decode '())
    (define-values (status headers response-port)
      (http-conn-recv! hc #:method #"GET" #:close? #t #:content-decode '()))

    (define new-url
      (ormap (λ (h)
               (match (regexp-match #rx#"^[Ll]ocation: (.*)$" h)
                 [#f #f]
                 [(list _ m1b)
                  (define m1 (bytes->string/utf-8 m1b))
                  (with-handlers ((exn:fail? (λ (x) #f)))
                    (define next-url (string->url m1))
                    (make-url
                     (or (url-scheme next-url) (url-scheme url))
                     (or (url-user next-url) (url-user url))
                     (or (url-host next-url) (url-host url))
                     (or (url-port next-url) (url-port url))
                     (url-path-absolute? next-url)
                     (url-path next-url)
                     (url-query next-url)
                     (url-fragment next-url)))]))
             headers))
    (define redirection-status-line?
      (regexp-match #rx#"^HTTP/[0-9]+[.][0-9]+ 3[0-9][0-9]" status))
    (cond
      [(and redirection-status-line? new-url (not (zero? redirections)))
       (redirection-loop (- redirections 1) new-url)]
      [else
       response-port])))
