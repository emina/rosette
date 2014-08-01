#lang s-exp "../../../lang/main.rkt"

(kernel void (sample [int* dst] [int* src] [int offset])
  (: int id)
  (= id (get_global_id 0))
  (= [dst id] (+ [src id] offset)))
