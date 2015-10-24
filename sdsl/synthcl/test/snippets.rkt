#lang s-exp "../lang/main.rkt"

(: int x)
(: char* y)
(: float3 z)
(: float v)
(: int16 u)

(assert (+ x 1))
(assert (% (+ x 2) 3))
(assert (!= x 2))

(?: x x z)
(?: z x x)

(assert (|| x (! v)))
(assert (! ((bool) x)))
(assert (== x v))

NULL
((int16) v)

(= x 3.4)
x

(+= z 2)
z

(%= x 3)
x

(int3 4 5 6)
(= [u xyz] (int3 4 5 6))
u

(+ (int3 1 2 3) 4)

((int4 5 6 7 8) s03)

(if x {}{})

(if x 
    { (= [u sf] 10) }
    { (= [u sf] 9) }
)
u

(if (! x) 
    { (: int g) (= g 3) (= [u sf] g) }
    { (= [u sf] 9) }
)
u

(for [(: int i in (range 0 4 1))] )
(for [(: int i in (range 0 4 1))] 
  (if (! x)
      { (: int g) (= g i) (+= [u sf] g)} )
  )
u


(: int16* w)
(= w ((int16*) (malloc 32)))
(= [w 0] 1)
(= [w 1] 2)
w
[w 0]
[w 1]

(get_work_dim)

(procedure void (nop1))
(nop1)
(kernel void (nop2))
(nop2)

(procedure int (int_iden [int x]) x)
(int_iden ((int) 4.5))
(int_iden #t)
(int_iden 4.5)



;;;;;; assertion failure localization ;;;;;;
; (assert #f)

;;;;;; bad types etc ;;;;;;
;(: float* NULL)
;(+ x y)
;(?: "" x x)
;((int) z)
;(-= z w)
;(%= z 3)
;(NULL 3)
;(if x)
;(for [() () "" (-= x 1)])
;[w ""]
;[w 2]
;(procedure int (bad))
;(procedure)
;(kernel int (bad) 1)
;(procedure void (w))
;(int_iden "")
;(procedure float (bad) "")
