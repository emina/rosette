#lang racket/base

;; Check whether z3 is installed during package setup.
;; If missing, builds & links a z3 binary.


(provide pre-installer)

(require racket/match
         racket/file
         racket/port
         racket/system)

;; -----------------------------------------------------------------------------

(define-syntax-rule (log message)
  (begin
    (display "pre-install: --- ")
    (display message)
    (displayln " ---")))

(define-syntax-rule (z3-build-error reason)
  (raise-user-error 'pre-installer "Failed to build z3 binary. ~a" reason))

(define (pre-installer collections-top-path racl-path)
  (define z3-bin-dest (build-path racl-path ".." "bin" "z3"))
  (unless (file-exists? z3-bin-dest)
    (make-directory* (build-path racl-path ".." "bin"))
    (define z3-bin-src (build-path racl-path ".." "z3" "build" "z3"))
    (unless (file-exists? z3-bin-src)
      (parameterize ([current-directory (build-path racl-path ".." "z3")])
        (build-z3-from-source)))
    (make-symlink z3-bin-src z3-bin-dest)))

;; Try to build z3 from source.
;; Assumes current location is the z3 git root
(define (build-z3-from-source)
  (log "configuring z3")
  (when (eq? 'win (system-type))
    (z3-build-error "Cannot automatically build z3 on Windows. Good luck."))
  (unless (file-exists? "configure")
    (z3-build-error "Could not locate './configure' script"))
  (unless (and (system* "configure") (directory-exists? "build"))
    (z3-build-error "Error running './configure'"))
  (parameterize ([current-directory "./build"])
    (log "building z3 from source")
    (unless (system "make")
      (z3-build-error "Error running './build/make'")))
  (displayln "Successfully built z3 binary"))

;; Create a symlink from `src` to `dst`.
;; Replace destination if it already exists.
(define (make-symlink src dst)
  (log "creating z3 symlink")
  (delete-directory/files dst #:must-exist? #f)
  (or
    (case (system-type)
     [(unix macosx)
      (system (format "ln -s ~a ~a" src dst))]
     [(win)
      ;; Yes, it's really DST before SRC
      (system (format "mklink ~a ~a" dst src))]
     [else
      (raise-user-error 'make-symlink "Unknown system type '~a'" (system-type))])
    (raise-user-error 'make-symlink "Failed to symlink '~a' to '~a'" src dst)))

;; Currently unused, but will be useful if Rosette is using a stable z3 release
(define (get-z3-url)
  (match (list (system-type) (system-type 'word))
    ['(unix 32)
     (values
      "https://github.com/Z3Prover/z3/releases/download/z3-4.4.1/z3-4.4.1-x86-ubuntu-14.04.zip"
       "z3-4.4.1-x86-ubuntu-14.04")]
    ['(unix 64)
     (values
      "https://github.com/Z3Prover/z3/releases/download/z3-4.4.1/z3-4.4.1-x64-ubuntu-14.04.zip"
      "z3-4.4.1-x64-ubuntu-14.04")]
    [`(macosx ,_)
     (values
      "https://github.com/Z3Prover/z3/releases/download/z3-4.4.1/z3-4.4.1-x64-osx-10.11.zip"
      "z3-4.4.1-x64-osx-10.11")]
    ['(win 32)
     (values
      "https://github.com/Z3Prover/z3/releases/download/z3-4.4.1/z3-4.4.1-x86-win.zip"
      "z3-4.4.1-x86-win")]
    ['(win 64)
     (values
      "https://github.com/Z3Prover/z3/releases/download/z3-4.4.1/z3-4.4.1-x64-win.zip"
      "z3-4.4.1-x64-win")]
    [any
     (raise-user-error 'get-z3-url "Unknown system type '~a'" any)]))
