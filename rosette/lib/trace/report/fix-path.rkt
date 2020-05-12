#lang racket

;; Parcel hashes path to avoid collision
;; (because it collapses a project into a flat directory).
;; However, we don't have a directory structure,
;; so remove the hash to minimize changes of compilation result
;; after each modification

(current-directory "dist")

;; rename files

(define changes
  (for/list ([old-path (in-list (directory-list "."))]
             #:when (equal? #".js" (path-get-extension old-path)))
    (define old-name (path->string old-path))
    (define new-name (regexp-replace #rx"\\..*\\." old-name "."))
    (rename-file-or-directory old-name new-name #t)
    (list old-name new-name)))

;; modify content to point to the correct filename

(for* ([path (in-list (directory-list "."))]
       [change (in-list changes)])
  (match-define (list old-name new-name) change)
  (define old-content (file->string path))
  (define new-content (string-replace old-content old-name new-name))
  (with-output-to-file path #:exists 'replace
    (thunk (display new-content))))
