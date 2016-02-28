#lang racket

(require rackunit rackunit/text-ui rosette/lib/roseunit
         (only-in "../model/memory.rkt" NULL)
         (only-in "../model/runtime.rkt" malloc)
         "../model/work.rkt"
         (only-in "../model/kernel.rkt" clSetKernelArg)
         (only-in rosette assert)
         "../lang/env.rkt" 
         "../lang/types.rkt"
         "../lang/typecheck.rkt"
         "../lang/sugar.rkt")

(define (fails-with? msg)
  (lambda (e)
    (and (exn:fail? e)
         (regexp-match (regexp-quote msg) (exn-message e))))) 

(define (check-type stx expected)
  (check-equal? (type-ref stx) expected))


(define env-tests
  (test-suite+ 
   "Tests for environment functions"
   (parameterize ([current-env (env)])
     (bind #'x int)
     (check-exn exn:fail? (thunk (bind #'x int)))
     (check-type (type-set #'x int) int)
     (check-type (lookup #'x) int)
     (check-exn exn:fail? (thunk (lookup #'y))))))

(define literal-tests
  (test-suite+ 
   "Tests for typechecking of literals"
   (parameterize ([current-env (env)])
     (check-type (typecheck #'#f) bool)
     (check-type (typecheck #'#t) bool)
     (check-type (typecheck #'10) int)
     (check-type (typecheck #'.5) float)
     (check-type (typecheck #'"foo") char*))))

(define declaration-tests
  (test-suite+ 
   "Tests for typechecking of declarations"
   (parameterize ([current-env (env)])
     (check-not-exn (thunk (typecheck #'(: int x))))
     (check-type (typecheck #'x) int)
     
     (check-not-exn (thunk (typecheck #'(: int16* y z))))
     (check-type (typecheck #'y) int16*)
     (check-type (typecheck #'z) int16*)
     
     (check-exn (fails-with? ":: not a valid type") 
                (thunk (typecheck #'(: void v))))
     
     (check-exn (fails-with? ":: expected (: type [len] x y ...) or (: type x y ...)") 
                (thunk (typecheck #'(: int))))
     
     (check-exn (fails-with? ":: not a valid type identifier") 
                (thunk (typecheck #'(: foo x))))
     
     (check-exn (fails-with? ":: duplicate declaration") 
                (thunk (typecheck #'(: int x)))))))

(define operator-tests
  (test-suite+
   "Tests for typechecking of operator expressions"
   (parameterize ([current-env (env)])
     (typecheck #'(: int a b c))
     (typecheck #'(: float x y z))
     (typecheck #'(: bool v w))  
     (typecheck #'(: int3 i3))
     (typecheck #'(: float3 f3))
     
     (define nary-real-ops (syntax->list #'(+ - *)))
     (define nary-int-ops (syntax->list #'(& $ ^ )))
     (define cmps (syntax->list #'(< <= > >= == !=)))
     (define nary-log-ops (syntax->list #'(&& ||)))
     
     (check-exn (fails-with? (~a (syntax->datum #'?:) ": wrong number of operands"))
                (thunk (typecheck #'(?: a b))))
     
     (for ([op nary-real-ops])
       (check-type (typecheck #`(#,op a b c)) int)
       (check-type (typecheck #`(#,op a b c v)) int)
       (check-type (typecheck #`(#,op a b c v x)) float)
       (check-type (typecheck #`(#,op a b c v i3)) int3)
       (check-type (typecheck #`(#,op a b c v f3)) float3)
       (check-exn (fails-with? (~a (syntax->datum op) ": no common real type"))
                  (thunk (typecheck #`(#,op a b c v x i3)))))
     
     (for ([op nary-int-ops])
       (check-type (typecheck #`(#,op a b c)) int)
       (check-type (typecheck #`(#,op a b c v)) int)
       (check-type (typecheck #`(#,op a b c v i3)) int3)
       (check-exn (fails-with? (~a (syntax->datum op) ": no common integer type for operands"))
                  (thunk (typecheck #`(#,op a b c v x)))))
     
     (for ([op cmps])
       (check-type (typecheck #`(#,op a b)) int)
       (check-type (typecheck #`(#,op a v)) int)
       (check-type (typecheck #`(#,op i3 b)) int3)
       (check-type (typecheck #`(#,op x f3)) int3)
       (check-exn (fails-with? (~a (syntax->datum op) ": no common real type"))
                  (thunk (typecheck #`(#,op i3 f3)))))
     
     (for ([op nary-log-ops])
       (check-type (typecheck #`(#,op v w)) bool)
       (check-exn (fails-with? (~a (syntax->datum op) ": no implicit conversion from int3 to bool"))
                  (thunk (typecheck #`(#,op i3 f3)))))
     )))

(define expression-tests
  (test-suite+
   "Tests for typechecking of expressions"
   (parameterize ([current-env (env)])
     (typecheck #'(: int a b c))
     (typecheck #'(: float x y z))
     (typecheck #'(: bool v))
     (typecheck #'(: int3 i3))
     (typecheck #'(: float3 f3))
     
     (typecheck #'(: int* p))
     (typecheck #'(: void* r))
     (typecheck #'(: cl_mem q))
     
     (check-type (typecheck #'(p 3)) int)
     (check-exn (fails-with? "p: expected int, given bool")
                (thunk (typecheck #'(p #f))))
     (check-exn (fails-with? "r: cannot dereference a void* pointer")
                (thunk (typecheck #'(r 0))))
     (check-exn (fails-with? "q: cannot dereference a void* pointer")
                (thunk (typecheck #'(q 0))))
     
     (check-type (typecheck #'(i3 xyzxyzxy)) int8)
     (check-type (typecheck #'(i3 s2)) int)
     (check-exn (fails-with? "i3: component selector out of bounds")
                (thunk (typecheck #'(i3 w))))
     
     (check-exn (fails-with? "a: expected procedure application, or vector/pointer access")
                (thunk (typecheck #'(a 3))))
     
     (check-exn (fails-with? "a: expected procedure application, or vector/pointer access")
                (thunk (typecheck #'(a 3 4)))))))

(define assignment-tests
  (test-suite+ 
   "Tests for typechecking of assignments"
   (parameterize ([current-env (env)])
     (typecheck #'(: int x))
     (typecheck #'(: int16 y))
     (typecheck #'(: float3 z u))
     (typecheck #'(: float2 v))
     (typecheck #'(: int* ptr))
     (typecheck #'(: void* vptr))
     (typecheck #'(: cl_mem mptr))
     
     (check-not-exn (thunk (typecheck #'(= x #f))))
     (check-not-exn (thunk (typecheck #'(= y 1)))) 
     (check-exn (fails-with? "=: no implicit conversion from int16 to float3") 
                (thunk (typecheck #'(= z y))))
     
     (check-exn (fails-with? "=: expected vector or pointer, given int") 
                (thunk (typecheck #'(= [x y] 1))))
     
     (check-not-exn (thunk (typecheck #'(= [ptr 3] 1))))
     (check-exn (fails-with? "=: expected int, given bool") 
                (thunk (typecheck #'(= [ptr #f] 1))))
     (check-not-exn (thunk (typecheck #'(= [ptr 3] 1))))
     (check-exn (fails-with? "=: no implicit conversion from float3 to int") 
                (thunk (typecheck #'(= [ptr 1] z))))
     (check-exn (fails-with? "=: cannot dereference a void* pointer") 
                (thunk (typecheck #'(= [vptr 1] z))))
     (check-exn (fails-with? "=: cannot dereference a void* pointer") 
                (thunk (typecheck #'(= [mptr 1] z))))
     
     (for ([sel (syntax->list #'(x y z s0 s1 s2))])
       (check-not-exn (thunk (typecheck #`(= [z #,sel] 1.01)))))
     (for ([sel (syntax->list #'(xy xz yx yz zx zy s01 s02 s10 s12 s20 s21))])
       (check-not-exn (thunk (typecheck #`(= [z #,sel] v)))))
     (for ([sel (syntax->list #'(xyz xzy yxz yzx zxy zyx s012 s021 s102 s120 s201 s210))])
       (check-not-exn (thunk (typecheck #`(= [z #,sel] u)))))
     
     (check-exn (fails-with? "=: component selector contains duplicates") 
                (thunk (typecheck #'(= [z xx] v))))
     (check-exn (fails-with? "=: wrong number of component selectors") 
                (thunk (typecheck #'(= [z s01234] v))))
     (check-exn (fails-with? "=: invalid component selector") 
                (thunk (typecheck #'(= [z s0x] v))))
     (check-exn (fails-with? "=: component selector out of bounds") 
                (thunk (typecheck #'(= [z s03] v))))
     (check-exn (fails-with? "=: expected float, given int") 
                (thunk (typecheck #'(= [z x] 1))))
     
     (for ([assgn (syntax->list #'(+= -= *= /= %= <<= >>= &= $= ^=))])
       (check-type (typecheck (desugar #`(#,assgn y x))) void))
     
     #|
     (for ([assgn (syntax->list #'(++ --))])
       (check-type (typecheck #`(#,assgn y)) int16)
       (check-type (typecheck #`(y #,assgn)) int16)
       (check-exn (fails-with? (~a (syntax->datum assgn) ": not an lvalue"))
                  (thunk (typecheck #`(#,assgn 1))))
       (check-exn (fails-with? (~a (syntax->datum assgn) ": not an lvalue"))
                  (thunk (typecheck #`(1 #,assgn))))
       (check-exn (fails-with? (~a (syntax->datum assgn) 
                                   ": expected an int scalar or vector, given float2"))
                  (thunk (typecheck #`(#,assgn v))))
       (check-exn (fails-with? (~a (syntax->datum assgn) 
                                   ": expected an int scalar or vector, given float2"))
                  (thunk (typecheck #`(v #,assgn)))))|#
     )))

(define cast-tests
  (test-suite+ 
   "Tests for typechecking of casts"
   (parameterize ([current-env (env)])
     (typecheck #'(: int x))
     (typecheck #'(: int16 y))
     (typecheck #'(: float3 z u))
     (typecheck #'(: bool v))
     (typecheck #'(: int* p))
     (typecheck #'(: cl_mem r))
     
     (for ([t (list bool int float int2 float2)])
       (check-type (typecheck #`((#,(type-name t)) x)) t))
     
     (for ([t (list int2* float* float2* void*)])
       (check-type (typecheck #`((#,(type-name t)) p)) t)
       (check-type (typecheck #`((#,(type-name t)) r)) t))
     
     (check-exn (fails-with? "?: no implicit conversion from int* to bool") 
                (thunk (typecheck #'((bool) p))))
     
     (check-exn (fails-with? "?: no implicit conversion from int* to float3") 
                (thunk (typecheck #'((float3) p))))
     
     (check-exn (fails-with? "?: no implicit conversion from int16 to int") 
                (thunk (typecheck #'((int) y))))
     
     (check-exn (fails-with? "?: no implicit conversion from float3 to int") 
                (thunk (typecheck #'((int) z)))))))

(define malloc-tests
  (test-suite+ 
   "Tests for typechecking of malloc"
   (parameterize ([current-env (env)])
     (check-type (typecheck #'(malloc 3)) void*)
     (check-exn (fails-with? "malloc: no implicit conversion from int2 to int")
                (thunk (typecheck #'(malloc (int2 3 3))))))))
     
   
(define assert-tests
  (test-suite+ 
   "Tests for typechecking of assertions"
   (parameterize ([current-env (env)])
     (typecheck #'(: int x))
     (typecheck #'(: int16 y))
     (typecheck #'(: float3 z u))
     (typecheck #'(: bool v))
     (typecheck #'(: int* p))
     (typecheck #'(: cl_mem r))     
     (check-not-exn (thunk (typecheck #'(assert v))))
     (check-not-exn (thunk (typecheck #'(assert x))))
     (check-exn (fails-with? "assert: no implicit conversion from int* to bool") 
                (thunk (typecheck #'(assert p)))))))

(define builtins-tests
  (test-suite+ 
   "Tests for typechecking of builtins"
   (parameterize ([current-env (env)])
     (typecheck #'(: cl_kernel k))
     (typecheck #'(: cl_mem buff))
     (check-type (typecheck #'(get_work_dim)) int)
     (check-type (typecheck #'(get_global_offset 3)) int)
     (check-type (typecheck #'(clSetKernelArg k 0 buff)) void)
     (check-type (typecheck #'(clSetKernelArg k 0 5)) void)
     (check-exn (fails-with? "get_global_offset: expected 1 arguments")
                (thunk (typecheck #'(get_global_offset 3 2))))
     (typecheck #'(: int* x))
     (check-exn (fails-with? "get_global_offset: no implicit conversion from int* to int")
                (thunk (typecheck #'(get_global_offset x)))))))

(time (run-tests env-tests))
(time (run-tests literal-tests))
(time (run-tests declaration-tests))
(time (run-tests expression-tests))
(time (run-tests operator-tests))
(time (run-tests assignment-tests))
(time (run-tests cast-tests))
(time (run-tests malloc-tests))
(time (run-tests assert-tests))
(time (run-tests builtins-tests))


