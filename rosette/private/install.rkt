#lang racket/base

;; Check whether z3 is installed during package setup.
;; If missing, builds & links a z3 binary.


(provide pre-installer post-installer)

(require racket/match
         racket/file
         racket/port
         net/url
         file/unzip)

; We need to run the Z3 installer as a pre-install step, because building the
; documentation relies on Z3 being available. But pre-install is so early in
; the build that its output gets pushed off screen by the later steps. So we
; use this little hack to repeat the failure message as a post-install step,
; which happens at the very end of the install and so makes the error message
; far more obvious.
(define z3-install-failure #f)

(define z3-version "4.8.8")

(define (print-failure path msg)
  (printf "\n\n********** Failed to install Z3 **********\n\n")
  (printf "Rosette installed successfully, but wasn't able to install the Z3 SMT solver.\n\n")
  (printf "You'll need to manually install a Z3 binary at this location:\n")
  (printf "    ~a\n" path)
  (printf "or anywhere that is on your PATH. Alternatively, in your programs, you can\n")
  (printf "construct a solver object manually:\n")
  (printf "    (current-solver (z3 #:path \"/path/to/z3\"))\n\n")
  (printf "Note that Rosette ships with a specific release of Z3 (v~a). Installing a\n" z3-version)
  (printf "different version of Z3 may change the performance of Rosette programs.\n\n")
  (printf "The problem was:\n    ~a\n\n" msg)
  (printf "**********\n\n\n"))

(define (post-installer collections-top-path)
  (match z3-install-failure
    [(cons z3-path msg) (print-failure z3-path msg)]
    [_ (void)]))

(define (pre-installer collections-top-path racl-path)
  (define bin-path (simplify-path (build-path racl-path ".." "bin")))
  (define z3-path (build-path bin-path "z3"))
  (with-handlers ([exn:fail? (lambda (e)
                               (set! z3-install-failure (cons z3-path (exn-message e)))
                               (print-failure z3-path (exn-message e)))])
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
  ; TODO: Z3 packages a macOS aarch64 binary as of 4.8.16, so remove this special case when we update
  ; to a newer Z3 version.
  (if (and (equal? (system-type 'os*) 'macosx) (equal? (system-type 'arch) 'aarch64))
      (values "https://github.com/emina/rosette/releases/download/4.1/z3-4.8.8-aarch64-osx-13.3.1.zip" "z3")
      (let ()
        (define site "https://github.com/Z3Prover/z3/releases/download")
        (define-values (os exe)
          (match (list (system-type 'os*) (system-type 'arch))
            ['(linux x86_64)   (values "x64-ubuntu-16.04" "z3")]
            [`(macosx ,_)      (values "x64-osx-10.14.6" "z3")]
            ['(windows x86_64) (values "x64-win" "z3.exe")]
            [any               (raise-user-error 'get-z3-url "No Z3 binary available for system type '~a" any)]))
        (define name (format "z3-~a-~a" z3-version os))
        (values
         (format "~a/z3-~a/~a.zip" site z3-version name)
         (format "~a/bin/~a" name exe)))))
