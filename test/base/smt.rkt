#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(define (extract fml)
  (define-values (in out) (make-pipe))
  (parameterize ([output-smt out])
    (solve (assert fml)))
  (close-output-port out)

  ;; drop 5 for
  ;; (reset)
  ;; (set-option :auto-config true)
  ;; (set-option :produce-unsat-cores false)
  ;; (set-option :smt.mbqi.max_iterations 10000000)
  ;; (set-option :smt.relevancy 2)
  ;;
  ;; drop-right 7 for
  ;; (check-sat)
  ;; (get-model)
  ;; and the other 5 mentioned above

  (drop-right (drop (port->list read in) 5) 7))

(define smt-tests
  (test-suite+
   "SMT tests"

   ;; a dummy call so that next tests start with (reset)
   (solve #t)

   (define-symbolic a b c d integer?)

   (check-equal?
    (extract (<= a b))
    '((declare-fun c0 () Int)
      (declare-fun c1 () Int)
      (define-fun e2 () Bool (<= c0 c1))
      (assert e2)))

   (check-equal?
    (extract (<= (+ a b) (- c)))
    '((declare-fun c0 () Int)
      (declare-fun c1 () Int)
      (define-fun e2 () Int (+ c0 c1))
      (declare-fun c3 () Int)
      (define-fun e4 () Int (- c3))
      (define-fun e5 () Bool (<= e2 e4))
      (assert e5)))

   (check-equal?
    (extract (<= (+ a b) (- (+ a b))))
    '((declare-fun c0 () Int)
      (declare-fun c1 () Int)
      (define-fun e2 () Int (+ c0 c1))
      (define-fun e3 () Int (- e2))
      (define-fun e4 () Bool (<= e2 e3))
      (assert e4)))

   (check-equal?
    (extract (<= (+ a b c d) (- (+ a b c d))))
    '((declare-fun c0 () Int)
      (declare-fun c1 () Int)
      (declare-fun c2 () Int)
      (declare-fun c3 () Int)
      (define-fun e4 () Int (+ c0 c1 c2 c3))
      (define-fun e5 () Int (- e4))
      (define-fun e6 () Bool (<= e4 e5))
      (assert e6)))

   (check-equal?
    (extract (<= (if (= a b) c d) (if (= a b) d c)))
    '((declare-fun c0 () Int)
      (declare-fun c1 () Int)
      (define-fun e2 () Bool (= c0 c1))
      (declare-fun c3 () Int)
      (declare-fun c4 () Int)
      (define-fun e5 () Int (ite e2 c3 c4))
      (define-fun e6 () Int (ite e2 c4 c3))
      (define-fun e7 () Bool (<= e5 e6))
      (assert e7)))))

(module+ test
  (time (run-tests smt-tests)))
