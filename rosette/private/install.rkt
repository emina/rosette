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
    (unless (file-exists? z3-path)
      (define-values (z3-url z3-extracted-subdir) (get-z3-url))
      (define z3-port (get-pure-port (string->url z3-url) #:redirections 10))  
      (make-directory* bin-path) ;; Ensure that `bin-path` exists
      (parameterize ([current-directory bin-path])    
        (call-with-unzip z3-port
                         (λ (dir)
                           (copy-directory/files (build-path dir "z3") z3-path)))
        ;; Unzipping loses file permissions, so we reset the z3 binary here
        (file-or-directory-permissions 
          z3-path 
          (if (equal? (system-type) 'windows) #o777 #o755))))))


;; Currently unused, but will be useful if Rosette is using a stable z3 release
(define (get-z3-url)
  (match (list (system-type) (system-type 'word))
    ['(unix 64)
     (values
      "https://github.com/emina/rosette/releases/download/v2.0/z3-d89c39cb-x64-ubuntu-14.04.zip"
      "z3-d89c39cb-x64-ubuntu-14.04")]
    [`(macosx ,_)
     (values
      "https://github.com/emina/rosette/releases/download/v2.0/z3-d89c39cb-x64-osx-10.11.zip"
      "z3-d89c39cb-x64-osx-10.11")]
    ['(windows 64)
     (values
      "https://github.com/emina/rosette/releases/download/v2.0/z3-d89c39cb-x64-win.zip"
      "z3-d89c39cb-x64-win")]
    [any
     (raise-user-error 'get-z3-url "Unknown system type '~a'" any)]))
