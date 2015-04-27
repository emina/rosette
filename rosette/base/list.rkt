#lang racket

(require (for-syntax racket/syntax "lift.rkt") 
         racket/provide racket/splicing racket/stxparam 
         "safe.rkt" "lift.rkt" "seq.rkt" 
         (only-in "control.rkt" @if @and @or @cond)
         (only-in "define.rkt" define-symbolic*)
         (only-in "term.rkt" term? define-type)
         (only-in "equality.rkt" @eq? @equal?)
         (only-in "any.rkt" @any?)
         (only-in "generic.rkt" make-cast)
         (only-in "bool.rkt" instance-of? and-&& && || =>)
         (only-in "num.rkt" @number? @<= @< @= @> @+)
         (only-in "union.rkt" union union?)
         (only-in "merge.rkt" merge merge*)
         (only-in "type.rkt" subtype?))

(provide (filtered-out with@ (all-defined-out))
         (rename-out [list @list] [null @null]))

;; Pair and List Predicates
(define (pair=? =?)
  (lambda (x y) 
    (and-&& (not (null? x)) (not (null? y)) (=? (car x) (car y)) (=? (cdr x) (cdr y)))))

; force? is ignored since pairs are immutable and therefore always merged
(define (pair/compress force? ps) 
  (match ps
    [(list _ ) ps]
    [(list (cons g (cons x y)) (cons h (cons u v))) 
     (list (cons (|| g h) (cons (merge* (cons g x) (cons h u))
                                (merge* (cons g y) (cons h v)))))]
    [_ (list (cons (apply || (map car ps))
                   (cons (apply merge* (for/list ([p ps]) (cons (car p) (cadr p)))) 
                         (apply merge* (for/list ([p ps]) (cons (car p) (cddr p)))))))]))

(define (list=? =?)
  (lambda (xs ys)
    (and (= (length xs) (length ys))
         (let loop ([xs xs] [ys ys] [eqs '()])
           (if (null? xs) 
               (apply && eqs)
               (let ([eq (=? (car xs) (car ys))])
                 (and eq (loop (cdr xs) (cdr ys) (cons eq eqs)))))))))

; force? is ignored since lists are immutable and therefore always merged 
(define (list/compress force? ps) 
  (seq-compress ps length map : [(for/seq head body) (for/list head body)]))

(define-type @pair?
  #:pred     (instance-of? pair? (and/c @pair? (not/c @null?)))      
  #:least-common-supertype (lambda (t) (if (or (eq? t @pair?) (eq? t @list?)) @pair? @any?))
  #:eq?      (pair=? @eq?)
  #:equal?   (pair=? @equal?)
  #:cast     (make-cast pair? @pair?)
  #:compress pair/compress
  #:construct (match-lambda [(list a b) (cons a b)]
                            [v (error 'construct-pair "expected a list of two elements, given ~a" v)])
  #:deconstruct (match-lambda [(cons a b) (list a b)]
                              [v (error 'deconstruct-pair "expected a pair, given ~a" v)]))

(define-type @list?  
  #:pred     (instance-of? list? @list?)      
  #:least-common-supertype (lambda (t) (cond [(eq? t @list?) @list?]
                                             [(eq? t @pair?) @pair?]
                                             [else @any?]))
  #:eq?      (list=? @eq?)
  #:equal?   (list=? @equal?)
  #:cast     (make-cast list? @list?)
  #:compress list/compress
  #:construct identity
  #:deconstruct identity)



;; Pair Constructors and Selectors
(define/lift (car cdr) :: pair? -> @pair?)

(define @null?
  (match-lambda [(? null?) #t]
                [(union vs (? (curry subtype? @list?)))
                 (apply || (for/list ([gv vs] #:when (null? (cdr gv))) (car gv)))]
                [_ #f]))

(define @cons
  (match-lambda** [(x (union ys)) (merge** ys (cons x _))]
                  [(x y) (cons x y)]))

;; List Operations
(define/lift (length reverse) :: list? -> @list?)
(define/lift/ref list-ref : (list? length) -> @list?)
(define/lift/append append : (list? list) -> @list?)

;; List Iteration
(define (bad-lengths-error name . args)
  (thunk (error name "all lists must have same size\n  given: ~a" (map ~.a args))))

(define (lengths xs)
  (match xs
    [(? list?) (set (length xs))]
    [(union vs)  (apply set (map (compose length cdr) vs))]))

(define (cast-length xs len)
  (match xs
    [(? list?) (values (= (length xs) len) xs)]
    [(union (list _ ... (cons g (and (? list? vs) (app length (== len)))) _ ...)) (values g vs)]
    [_ (values #f xs)]))

(define-syntax (define/lift/iterator stx)
  (syntax-case stx () 
    [(_ iterator init ...) 
     #`(define #,(lift-id #'iterator) 
         (case-lambda
           [(proc init ... xs)
            (assert-arity-includes proc (+ (length (list init ...)) 1) (quote iterator))
            (lift/apply/higher-order iterator proc init ... xs : list? -> @list?)]
           [(proc init ... xs . rest)
            (assert-arity-includes proc (+ (length (list init ...)) 1 (length rest)) (quote iterator))
            (define name (quote iterator))
            (let ([vs (cons (coerce xs @list? name) (map (curryr coerce @list? name) rest))])
              (if (andmap list? vs) 
                  (apply iterator proc init ... vs)
                  (match (apply set-intersect (map lengths vs))
                    [(? set-empty?) (assert #f (apply bad-lengths-error name xs rest))]
                    [lens (let loop ([lens (sort (set->list lens) <)])
                            (match lens
                              [(list len) 
                               (let-values ([(gs ys) (for/lists (gs ys) ([v vs]) (cast-length v len))])
                                 (assert (apply && gs) (apply bad-lengths-error name xs rest))
                                 (apply iterator proc init ... ys))]
                              [(list len rest (... ...))
                               (let-values ([(gs ys) (for/lists (gs ys) ([v vs]) (cast-length v len))])
                                 (@if (apply && gs)
                                         (apply iterator proc init ... ys)
                                         (loop rest)))]))])))]))]))

(define/lift/iterator map) 
(define/lift/iterator for-each)
(define/lift/iterator foldl init) 
(define/lift/iterator foldr init)

(define-syntax-parameter iterator-next (syntax-rules ()))

(define-syntax-rule (define-iterator id rule ...)
  (define id 
    (syntax-parameterize
     ([iterator-next (syntax-rules () rule ...)])
     (case-lambda
       [(f l) 
        (assert-arity-includes f 1 (quote id))
        (if (null? l)
            (iterator-next)
            (let loop ([l l])
              (iterator-next l (f (car l)) (loop (cdr l)))))]
       [(f l1 l2) 
        (assert-arity-includes f 2 (quote id))
        (assert (= (length l1) (length l2)) (bad-lengths-error (quote id) l1 l2))
        (if (null? l1)
            (iterator-next)
            (let loop ([l1 l1][l2 l2])
              (iterator-next l1 (f (car l1) (car l2)) (loop (cdr l1) (cdr l2)))))]
       [(f l . args) 
        (assert-arity-includes f (add1 (length args)) (quote id))
        (assert (andmap (curry = (length l)) args) (apply bad-lengths-error (quote id) l args))
        (if (null? l)
            (iterator-next)
            (let loop ([l l] [args args])
              (iterator-next l (apply f (car l) (map car args)) (loop (cdr l) (map cdr args)))))]))))


(splicing-local 
    [(define-iterator andmap [(_) (@and)] [(_ l cur rest) (@if (null? (cdr l)) cur (@and cur rest))])
     (define-iterator ormap  [(_) (@or)]  [(_ l cur rest) (@if (null? (cdr l)) cur (@or cur rest))])
     (define-iterator filter-map [(_) null]  [(_ l cur rest) (@if (null? l)
                                                                     null
                                                                     (let ([x cur])
                                                                       (@if x (@cons x rest) rest)))])]
  (define/lift/iterator andmap)
  (define/lift/iterator ormap)
  (define/lift/iterator filter-map))

;; List Filtering
(define-syntax (define/lift/applicator stx)
  (syntax-case stx () 
    [(_ id #:name name #:using (applicator arg ... xs))  
     #`(define (id name arg ... xs)
         (lift/apply/higher-order applicator arg ... xs : name : list? -> @list?))]
     [(_ applicator arg ... xs)
     #`(define (#,(lift-id #'applicator) arg ... xs)
         (lift/apply/higher-order applicator arg ... xs : list? -> @list?))]))
     
(splicing-local 
    [(define (filter f list)
       (assert-arity-includes f 1 'filter)
       (let loop ([l list] [result null])
         (@if (null? l)
             (@reverse result)
             (loop (cdr l) (@if (f (car l)) (@cons (car l) result) result)))))
     (define (filter-not f list)
       (assert-arity-includes f 1 'filter)
       (let loop ([l list] [result null])
         (@if (null? l)
             (@reverse result)
             (loop (cdr l) (@if (f (car l)) result (@cons (car l) result))))))]
  (define/lift/applicator filter f list)
  (define/lift/applicator filter-not f list))
     
(splicing-local
    [(define (do-remove equal? item list)
       (let loop ([list list])
         (@cond [(null? list) null]
                   [(equal? item (car list)) (cdr list)]
                   [else (@cons (car list) (loop (cdr list)))])))
     (define/lift/applicator @do-remove #:name name #:using (do-remove equal? item list))]
  (define @remove 
    (case-lambda
      [(item list) (@do-remove 'remove @equal? item list)]
      [(item list equal?) (assert-arity-includes equal? 2 'remove) 
                          (@do-remove 'remove equal? item list)]))
  (define (@remq item list) 
    (@do-remove 'remq @eq? item list)))

(splicing-local
    [(define (do-remove* equal? l r)
       (let rloop ([r r])
          (@cond
           [(null? r) null]
           [else (let ([first-r (car r)])
                   (let loop ([l-rest l])
                     (@cond
                      [(null? l-rest) (@cons first-r (rloop (cdr r)))]
                      [(equal? (car l-rest) first-r) (rloop (cdr r))]
                      [else (loop (cdr l-rest))])))])))
     (define (@do-remove* name equal? l r)
         (match* ((coerce l @list? name) (coerce r @list? name))
           [((? list? vs) (? list? ws)) (do-remove* equal? vs ws)]
           [((? list? vs) (union ws)) 
            (higher-order/for [ws] #:lift (do-remove* equal? vs) #:enforce @list? #:name name)]
           [((union ws) vs) 
            (let loop ([ws ws])
              (match ws
                [(list (cons g ys)) 
                 (assert g (type-error name @list? ys)) 
                 (@do-remove* name equal? ys vs)]
                [(list (cons g ys) rest  ...) 
                 (@if g (@do-remove* name equal? ys vs) (loop rest))]))]))]
  (define @remove* 
    (case-lambda
      [(l r) (@do-remove* 'remove* @equal? l r)]
      [(l r equal?) (assert-arity-includes equal? 2 'remove) 
                    (@do-remove* 'remove* equal? l r)]))
  (define (@remq* l r) 
    (@do-remove* 'remq* @eq? l r)))

(splicing-local
    [(define (concrete? less? keys)
       (match keys
         [(or (list) (list _)) #t]
         [(list key rest ...)  (and (for/and ([r rest])
                                      (let ([lt (less? key r)])
                                        (not (or (term? lt) (union? lt)))))
                                    (concrete? less? rest))]))
     (define (rank-sort less? getkey cache-keys? xs)
       (define key-of (cond [(and getkey cache-keys?) (curry hash-ref (for/hash ([x xs]) (values x (getkey x))))]
                            [(not getkey) identity]
                            [else getkey]))
       (cond [(concrete? less? (map key-of xs)) (fast-sort less? getkey cache-keys? xs)]
             [else 
              (define len (length xs))
              (let* ([ranked>? (lambda (x i y j) (@or (@and (@equal? x y) (> i j)) (less? y x)))]
                     [ranks (for/list ([(x i) (in-indexed xs)])
                              (for/fold ([rank 0]) ([(y j) (in-indexed xs)] #:unless (= i j))
                                (@+ rank (@if (ranked>? (key-of x) i (key-of y) j) 1 0))))])
                (for/list ([i len])
                  (for/fold ([v 0]) ([x xs] [r ranks]) (merge (@= i r) x v))))]))
              #|(define vars (for/list ([i (in-range len)]) (define-symbolic* rank @number?) rank))
              (for ([v vars])
                (assert (@<= 0 v))
                (assert (@< v len)))
              (let loop ([vars vars] [xs l])
                (match* (vars xs)
                  [((or (list) (list _)) _) (void)]
                  [((list v v-rest ...) (list x x-rest ...)) 
                   (let ([key (key-of x)])
                     (for ([v1 v-rest] [x1 x-rest])
                       (assert (@if (less? key (key-of x1)) (@< v v1) (@< v1 v)))))
                   (loop v-rest x-rest)]))
              (for/list ([i (in-range (length l))])
                (apply merge* (for/list ([x l] [v vars]) (cons (@= v i) x))))]))|#
     (define (fast-sort less? getkey cache-keys? xs)
       (sort xs less? #:key getkey #:cache-keys? cache-keys?))
     (define/lift/applicator fast-sort less? getkey cache-keys? xs)
     (define/lift/applicator rank-sort less? getkey cache-keys? xs)]
  (define (@sort xs less? #:key [getkey #f] #:cache-keys? [cache-keys? #f] #:ignore-symbolic? [ignore-term? #f])
       (cond [ignore-term? (@fast-sort less? getkey cache-keys? xs)]
             [else (assert-arity-includes less? 2 'sort)
                   (when getkey (assert-arity-includes getkey 1 'sort))
                   (@rank-sort less? getkey cache-keys? xs)])))

;; List Searching
(splicing-local
    [(define (memf f list)
       (assert-arity-includes f 1 'memf) 
       (let loop ([l list])
         (@cond
          [(null? l) #f]
          [(not (pair? l)) (assert #f (type-error 'memf @list? list))]
          [else (@if (f (car l)) l (loop (cdr l)))])))
     (define (findf f list)
       (assert-arity-includes f 1 'findf) 
       (let loop ([l list])
         (@cond
          [(null? l) #f]
          [(not (pair? l)) (assert #f (type-error 'findf @list? list))]
          [else (let ([a (car l)]) (@if (f a) a (loop (cdr l))))])))]
  (define/lift/applicator memf f list)
  (define/lift/applicator findf f list)
  (define (@member x xs) (@memf (curry @equal? x) xs))
  (define (@memq x xs) (@memf (curry @eq? x) xs))
  (define @assoc (case-lambda [(x xs) (@findf (compose (curry @equal? x) @car) xs)]
                              [(x xs eq?) (assert-arity-includes eq? 2 'assoc)
                                          (@findf (compose (curry eq? x) @car) xs)]))
  (define (@assq x xs) (@assoc x xs @eq?))
  (define (@assf proc xs) (@findf (compose proc car) xs)))

;; Pair and List Accessor Shorthands
(define/lift (caar cdar)       : (flat-pattern-contract (cons (? pair?) _)) -> @pair?) 
(define/lift (cadr cddr)       : (flat-pattern-contract (cons  _ (? pair?))) -> @pair?)
(define/lift (caaar cdaar)     : (flat-pattern-contract (cons (cons (? pair?) _) _)) -> @pair?)
(define/lift (caadr cdadr)     : (flat-pattern-contract (cons _ (cons (? pair?) _))) -> @pair?)
(define/lift (cadar cddar)     : (flat-pattern-contract (cons (cons _ (? pair?)) _)) -> @pair?) 
(define/lift (caddr cdddr)     : (flat-pattern-contract (cons _ (cons _ (? pair?)))) -> @pair?)
(define/lift (caaaar cdaaar)   : (flat-pattern-contract (cons (cons (cons (? pair?) _) _) _)) -> @pair?)
(define/lift (caaadr cdaadr)   : (flat-pattern-contract (cons _ (cons (cons (? pair?) _) _))) -> @pair?)
(define/lift (caadar cdadar)   : (flat-pattern-contract (cons (cons _ (cons (? pair?) _)) _)) -> @pair?)
(define/lift (caaddr cdaddr)   : (flat-pattern-contract (cons _ (cons _ (cons (? pair?) _)))) -> @pair?)
(define/lift (cadaar cddaar)   : (flat-pattern-contract (cons (cons (cons _ (? pair?)) _) _)) -> @pair?)
(define/lift (cadadr cddadr)   : (flat-pattern-contract (cons _ (cons (cons _ (? pair?)) _))) -> @pair?)
(define/lift (caddar cdddar)   : (flat-pattern-contract (cons (cons _ (cons _ (? pair?))) _)) -> @pair?)
(define/lift (cadddr cddddr)   : (flat-pattern-contract (cons _ (cons _ (cons _ (? pair?))))) -> @pair?)

(define/lift (last-pair)       : pair? -> @pair?)
(define/lift (first rest last) : (and/c list? (not/c empty?)) -> @list?)

(define/lift second            : (flat-pattern-contract (list _ __2)) -> @list?)
(define/lift third             : (flat-pattern-contract (list _ __3)) -> @list?)
(define/lift fourth            : (flat-pattern-contract (list _ __4)) -> @list?)
(define/lift fifth             : (flat-pattern-contract (list _ __5)) -> @list?)
(define/lift sixth             : (flat-pattern-contract (list _ __6)) -> @list?)
(define/lift seventh           : (flat-pattern-contract (list _ __7)) -> @list?)
(define/lift eighth            : (flat-pattern-contract (list _ __8)) -> @list?)
(define/lift ninth             : (flat-pattern-contract (list _ __9)) -> @list?)
(define/lift tenth             : (flat-pattern-contract (list _ __10)) -> @list?)

;; Additional List Functions and Synonyms
(define (extract vs idx proc guard)
  (apply merge* (let loop ([i 0] [out '()])
                (with-handlers ([exn:fail? (lambda (e) 
                                             (assert (=> guard (&& (@<= 0 idx) (@< idx i)))
                                                     (index-too-large-error (object-name proc) vs idx)) 
                                             out)])
                  (loop (add1 i) (cons (cons (@= i idx) (proc vs i)) out))))))

(define-syntax (define/lift/extractor stx)
  (syntax-case stx ()
    [(_ (id0 id ...))
     #'(begin (define/lift/extractor id0)
              (define/lift/extractor id) ...)]
    [(_ proc)
     #`(define (#,(lift-id #'proc) xs pos)
         (define name (object-name proc))
         (match* (xs (coerce pos @number? name))
           [((union vs) (? number? idx))
            (assert-bound [0 <= idx] name)
            (apply merge* (assert-some 
                         (let loop ([vs vs] [out '()])
                           (match vs
                             [(list) out]
                             [(list (cons g v) rest (... ...)) 
                              (with-handlers ([exn:fail? (lambda (e) (loop rest out))])
                                (loop rest (cons (cons g (proc v idx)) out)))]))
                         #:unless (length vs)
                         (index-too-large-error name vs idx)))]
           [((union vs) idx)
            (apply merge* (for/list ([v vs]) 
                          (cons (car v) (extract (cdr v) idx proc (car v)))))]
           [(vs (? number? idx)) 
            (proc vs idx)]
           [(vs idx)
            (extract vs idx proc #t)]))]))

(define/lift/extractor (list-tail take drop take-right drop-right))

(define/lift/split split-at @take @drop)
(define/lift/split split-at-right @drop-right @take-right)

(define/lift (shuffle) :: list? -> @list?)

(define @empty? @null?)
(define @cons? @pair?)

(define @flatten
  (match-lambda [(union vs) (merge** vs flatten)]
                [other (flatten other)]))

(define @append*
  (case-lambda [(ls) (@apply @append ls)] ; optimize common case
               [(l1 l2) (@apply @append l1 l2)]
               [(l1 l2 l3) (@apply @append l1 l2 l3)]
               [(l1 l2 l3 l4) (@apply @append l1 l2 l3 l4)]
               [(l . lss) (@apply @apply @append l lss)]))

(define (@add-between l x #:splice? [sp? #f] #:before-first [bf '()] #:before-last [bl x] #:after-last [al '()])
  (if (list? l)
      (add-between l x #:splice? sp? #:before-first bf #:before-last bl #:after-last al)
      (match (coerce l @list? 'add-between)
        [(? list? vs) (add-between vs x #:splice? sp? #:before-first bf #:before-last bl #:after-last al)]
        [(union vs) (merge** vs (add-between _ x #:splice? sp? #:before-first bf #:before-last bl #:after-last al))])))


(define @apply 
  (case-lambda [() (error 'apply "arity mismatch;\n  expected: at least 2\n  given: 0")]
               [(proc) (error 'apply "arity mismatch;\n  expected: at least 2\n  given: 1")]
               [(proc xs) (lift/apply/higher-order apply proc xs : list? -> @list?)]
               [(proc x0 xs) (lift/apply/higher-order apply proc x0 xs : list? -> @list?)]
               [(proc x0 x1 xs) (lift/apply/higher-order apply proc x0 x1 xs : list? -> @list?)]
               [(proc x0 x1 x2 xs) (lift/apply/higher-order apply proc x0 x1 x2 xs : list? -> @list?)]
               [(proc x0 x1 x2 x3 xs) (lift/apply/higher-order apply proc x0 x1 x2 x3 xs : list? -> @list?)]
               [(proc . xss) (@apply (apply curry proc (take xss (- (length xss) 1))) (last xss))]))
                               
(define @append-map
  (case-lambda [(f l) (@apply @append (@map f l))]
               [(f l1 l2) (@apply @append (@map f l1 l2))]
               [(f l . ls)  (@apply @append (apply @map f l ls))]))

(define @count 
  (case-lambda [(f l) (@apply @+ (@map (lambda (v) (@if (f v) 1 0)) l))]
               [(f l1 l2) (@apply @+ (@map (lambda (v1 v2) (@if (f v1 v2) 1 0)) l1 l2))]
               [(f l . ls) (@apply @+ (apply @map (lambda (v . vs) (@if (apply f v vs) 1 0)) l ls))]))

(splicing-local
    [(define (remove-dups =? key l)
       (let loop ([l l] [seen null])
         (@if (null? l)
                 l
                 (let* ([x (car l)] [k (key x)] [l (cdr l)])
                   (@if (@memf (curry =? k) seen)
                           (loop l seen)
                           (@cons x (loop l (@cons k seen))))))))
     (define/lift/applicator remove-dups =? key l)]
  (define (@remove-duplicates  l [=? @equal?] #:key [key identity])
    (@remove-dups =? key l)))

(define (@partition pred lst) (values (@filter pred lst) (@filter-not pred lst)))	

(splicing-local
    [(define (mk-min cmp name f xs)
       (assert-arity-includes f 1 name)
       (assert (@pair? xs) (argument-error name "(and/c list? (not/c empty?))" xs))
       
       (let ([init-min-var (coerce (f (car xs)) @number? name)])
         (let loop ([min (car xs)]
                    [min-var init-min-var]
                    [xs (cdr xs)])
           (@if (null? xs) 
                   min 
                   (let ([new-min (coerce (f (car xs)) @number? name)])
                     (@if (cmp new-min min-var)
                             (loop (car xs) new-min (cdr xs))
                             (loop min min-var (cdr xs))))))))
     (define/lift/applicator mk-min cmp name f xs)] 
  (define (@argmin f xs) (@mk-min @< 'argmin f xs))
  (define (@argmax f xs) (@mk-min @> 'argmax f xs)))

(splicing-local
    [(define (insert xs i v)
       (let-values ([(left right) (split-at xs i)])
         (append left (cons v right))))
     (define (insert* xs i v)
       (apply merge* 
              (cons (@= i (length xs)) (append xs (list v)))
              (for/list ([(x idx) (in-indexed xs)])
                     (cons (@= i idx) (insert xs idx v)))))]
  (define (@insert xs i v)
    (or (and (list? xs) (number? i) (insert xs i v))
        (match* ((coerce xs @list? 'insert) (coerce i @number? 'insert))
          [((? list? xs) (? number? i)) (insert xs i v)]
          [((? list? xs) i) 
           (assert-bound [0 @<= i @<= (length xs)] 'insert)
           (insert* xs i v)]
          [((union ys) (? number? i))
           (assert-bound [0 <= i] 'insert)
           (apply merge* (assert-some 
                        (for/list ([y ys] #:when (<= i (length (cdr y))))
                          (cons (car y) (insert (cdr y) i v)))
                        #:unless (length ys)
                        (index-too-large-error 'insert xs i)))]
          [((union ys) i)
           (assert-bound [0 @<= i @<= (@length xs)] 'insert)
           (merge** ys (insert* _ i v))]))))
           
(splicing-local
    [(define (replace xs i v)
       (let-values ([(left right) (split-at xs i)])
         (append left (cons v (cdr right)))))
     (define (replace* xs i v)
       (apply merge* (for/list ([(x idx) (in-indexed xs)])
                     (cons (@= i idx) (replace xs idx v)))))]
  (define (@replace xs i v)
    (or (and (list? xs) (number? i) (replace xs i v))
        (match* ((coerce xs @list? 'replace) (coerce i @number? 'replace))
          [((? list? xs) (? number? i)) (replace xs i v)]
          [((? list? xs) i) 
           (assert-bound [0 @<= i @< (length xs)] 'replace)
           (replace* xs i v)]
          [((union ys) (? number? i))
           (assert-bound [0 <= i] 'replace)
           (apply merge* (assert-some 
                        (for/list ([y ys] #:when (< i (length (cdr y))))
                          (cons (car y) (replace (cdr y) i v)))
                        #:unless (length ys)
                        (index-too-large-error 'replace xs i)))]
          [((union ys) i)
           (assert-bound [0 @<= i @< (@length xs)] 'replace)
           (merge** ys (replace* _ i v))]))))
           

#|
(define (test iterator size)
  (define-symbolic* n @number?)
  (define r (@if (@= n 3) (build-list size identity) (build-list (* 2 size) add1)))
  (define p (@if (@= n 2) (build-list size add1) (build-list (* 2 size) identity)))
  (time (iterator r p)))

(define-symbolic* n @number?)
(@andmap identity (list (@= 3 n) 4 (@= 5 n) 6))

(require (only-in "bool.rkt" @boolean?))
(define-symbolic* b c @boolean?)
(@andmap identity (list b c 1))
|#

#|
(require rosette/base/define) 
(require (only-in "bool.rkt" @boolean?))
(define-symbolic b @boolean?)
(define-symbolic i @number?)
(define xs '(a b c d e f g h i j k l))
(define ys '(n q))
(define v 'm)

(define (test-insert)
  (displayln (@insert xs (length xs) v))
  (displayln (@insert ys i v))
  (for ([i 4])
    (displayln (@insert (@if b xs ys) 0 v)))
  (displayln (@insert (@if b xs ys) i v)))

(define (test-replace)
  (displayln (@replace xs (sub1 (length xs)) v))
  (displayln (@replace ys i v))
  (for ([i 4])
    (displayln (@replace (@if b xs ys) 0 v)))
  (displayln (@replace (@if b xs ys) i v)))
  
  
(define (compare-inserts xs i v)
  (time (begin (@insert xs i v) (void)))
  (time (begin (let-values ([(left right) (@split-at xs i)])
                 (@append left (@cons v right)))
               (void))))|#
