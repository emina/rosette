#lang racket/base

(provide pre-installer)

(require racket/match
         racket/file
         racket/port
         net/url
         file/unzip)

(define (pre-installer collections-top-path racl-path)
  (define-values (z3-url z3-extracted-subdir)
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
        "z3-4.4.1-x64-win")]))
  (define z3-port (get-pure-port (string->url z3-url) #:redirections 10))
  (define bin-path (build-path racl-path ".." "bin"))
  (make-directory* bin-path)
  (parameterize ([current-directory bin-path])
    (unless (directory-exists? "z3")
      (call-with-unzip z3-port
                       (λ (dir)
                         (copy-directory/files (build-path dir z3-extracted-subdir)
                                               "z3")))
      (file-or-directory-permissions (build-path "z3" "bin" "z3")
                                     #o555))))

