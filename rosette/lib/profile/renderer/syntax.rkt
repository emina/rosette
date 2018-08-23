#lang racket

(require racket/date racket/path)
(provide make-folder-name syntax-srcloc procedure-name)

; Helpers to construct filenames
(define initial-date
  (match-let ([pad (lambda (x n) (~r x #:min-width n #:pad-string "0"))]
              [(date s m h d M y _ _ _ _) (current-date)])
    (string-append (pad y 4) (pad M 2) (pad d 2) (pad h 2) (pad m 2) (pad s 2))))
(define (syntax-srcfile stx)
  (match stx
    [(and (? syntax?) (app syntax-source (? path?)))
     (let-values ([(base name dir?) (split-path (syntax-source stx))])
       (path->string name))]
    [`(submod (file ,path) ,mod)
     (let-values ([(base name dir?) (split-path path)])
       (format "~a-~a" name mod))]
    [`(file ,path)
     (let-values ([(base name dir?) (split-path path)])
       (path->string name))]
    [(? path-string?)
     (let-values ([(base name dir?) (split-path stx)])
       (path->string name))]
    [_ "unknown"]))
(define make-folder-name
  (let ([n 0])
    (lambda (source)
      (begin0
        (format "~a-~a-~v" (syntax-srcfile source) initial-date n)
        (set! n (+ n 1))))))

; Get a version of a syntax object's source location that can be rendered
(define (path->pretty-path path)
  (path->string (find-relative-path (current-directory) path)))
(define (syntax-srcloc stx)
  (cond [(and (syntax? stx) (path? (syntax-source stx)))
         (format "~a:~v:~v" (path->pretty-path (syntax-source stx)) (syntax-line stx) (syntax-column stx))]
        [(and (list? stx) (= (length stx) 3) (not (eq? (first stx) 'submod)))
         (match-let* ([(list src line col) stx]
                      [name (if (path? src) (path->pretty-path src) (~a src))])
           (format "~a:~v:~v" name line col))]
        [stx (~a stx)]
        [else stx]))

; Get the name of a procedure
(define (procedure-name proc)
  (~a (or (object-name proc) proc)))