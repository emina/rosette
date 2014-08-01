#lang s-exp "../../../lang/main.rkt"


(kernel void (sample [int4* dst] [int4* src] [int4 offset])
  (: int id idx)
  (= id (get_global_id 0))
  (= idx (choose id (/ id 4) (% id 4) (+ id 4) (* id 4) (- id 4)))
  (= [dst idx] (+ [src idx] offset)))
