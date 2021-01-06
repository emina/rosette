#lang s-exp "../lang/main.rkt"

(: void* dst)
(: int SIZE)

(= SIZE 2)

(= dst ((int*) (malloc (* SIZE (sizeof int)))))

(for () (print "hello\n"))

(for [(: int x in (range 4))
      (: int y in (range 0 6 3))
      (: int z in (range 1))]
  (print x " " y " " z "\n"))

(procedure int (tiny0 [int x])
  (minus x))

(procedure int (tiny1 [int x])
  (minus x))

(grammar int (minus [int x])
  (- x (choose 0 1)))

(procedure int (foo [int x]) (locally-scoped (- ((int) x) 1)))

(synth #:forall [(: int x)]
       #:ensure (assert (&& (== x (tiny0 x)) 
                            (== (- x 1) (tiny1 x)))))

(synth #:forall [(: int k)
                 (: int t in (range 1 5))
                 (: int p in (range t 5))]
       #:ensure (if (&& (== t 3) (== p 4)) 
                    {(assert (choose k 3))}))

(verify #:forall [(: int t in (range 1 5))
                  (: int k)
                  (: int p in (range t 5))]
        #:ensure (if (&& (== t 2) (== p 4)) 
                     {(assert k)}))