#lang s-exp rosette

(require rosette/solver/smt/cvc4)
(current-solver (new cvc4%))

(define-symbolic x string?)
(define-symbolic y string?)
(define-symbolic z number?)

(define (show msg ex . strs)
  (define model (solve (apply ex strs)))
  (apply printf msg (map model strs)))

(define (ex1 x)
  (assert (equal? (string-append x "ab") (string-append "ba" x)))
  (assert (= (string-length x) 7)))

(define (ex2 x y)
  (assert (not (equal? x y)))
  (assert (= (string-length x) 3))
  (assert (= (string-length x) (string-length y))))

(define (ex3 x y)
  (assert (not (equal? (string-append x y) (string-append y x)))))

(define (ex4 x z)
  (assert (and (> z 0) (= (str-to-int x) z))))

(define (ex5 x z)
  (assert (and (> z 0) (equal? (int-to-str z) x))))

(show "Find an assignment for x, where x.\"ab\"=\"ba\".x and the length of x equals to 7:\n x = ~s\n" ex1 x)
(show "Find assignments for x and y, where x and y are distinct and their lengths are equal:\n x = ~s, y = ~s\n" ex2 x y)
(show "Find assignments for x and y, where x.y != y.x.\n x = ~s, y = ~s\n" ex3 x y)
(show "Find a string and its numerical value, not equal to 0.\n x = ~s, z = ~s\n" ex4 x z)
(show "Find a string and its numerical value, not equal to 0.\n x = ~s, z = ~s\n" ex5 x z)

     



