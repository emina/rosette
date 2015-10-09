#lang racket

(require (only-in "identifiers.rkt" syntax-identifier) 
         (only-in rosette/lib/util/syntax read-module 
                  location location-source location-contains?)
         (only-in rosette/base/util/ord-dict ord-dict)
         (only-in rosette/query/state current-solution))

(provide add-generator! tagged? generate-forms generate-expressions)

(define generators (ord-dict))
 
(define (add-generator! context proc)
  ;(printf "add-generator! ~a\n" context)
  (let* ([trace (map node context)]
         [tree (dict-ref! generators (last trace) ord-dict)])
    (unless (dict-has-key? tree trace)
      (dict-set! tree trace proc))))

(define (tagged? tags) 
  (lambda (id) (member (syntax->datum id) tags)))

(define (generate-forms [sol (current-solution)] #:filter [generate? any/c])
  (let* ([synths  (generate-expressions sol #:filter generate?)]
         [sources (map (compose1 location car) synths)]
         [synths  (for/hash ([loc sources] [synth synths])
                    (values loc (cdr synth)))]
         [sources (map read-module (remove-duplicates (map location-source sources)))]
         [has-synth? (lambda (loc) 
                       (for/or ([synth-loc (in-hash-keys synths)])
                         (location-contains? loc synth-loc)))])
    (apply 
     append
     (for/list ([source sources])       
       (syntax-case source () 
         [(mod id lang (mod-begin forms ...))
          (for/list ([form (syntax->list #'(forms ...))]
                     #:when (has-synth? (location form)))
            (cons (car (or (syntax->list form) (list form))) 
                  (generate-form form synths)))]
         [_ (error 'generate-forms "expected a module, given ~a" source)])))))

(define (generate-form stx loc->expr)
  (syntax-case stx ()
    [(head _ ...) 
     (or (hash-ref loc->expr (location #'head) #f)
         (quasisyntax/loc stx 
           (#,@(for/list ([form (syntax->list stx)])
                 (generate-form form loc->expr)))))]
    [_ stx]))
        
(define (generate-expressions [sol (current-solution)] #:filter [generate? any/c])
  (parameterize ([current-solution sol])
    (filter pair? 
     (for/list ([(root gens) (in-dict generators)] 
                #:when (generate? (node-identifier root)))
       (let ([expr (generate-expression root gens)])
         (and expr (cons (node-identifier root) expr)))))))

(define (generate-expression root generators)
  
  (define (generate path)
    (let* ([gen  (dict-ref generators path #f)]
           [expr (and gen (gen (curryr recurse path)))])
      (and (syntax? expr) (skip-identity expr))))
  
  (define (recurse stx path)
    (match stx
      [(app syntax->list (list head rest ...))
       (or (and (identifier? head) (generate (cons (node head) path)))
           (quasisyntax/loc stx 
             (#,(recurse head path) #,@(map (curryr recurse path) rest))))]
      [_ stx]))
  
  (generate (list root)))

(define (skip-identity stx)
  (syntax-case stx ()
    [(id e)
     (and (identifier? #'id) (free-identifier=? #'id #'identity))
     #'e]
    [_ stx]))

(struct node (identifier) 
  #:guard (lambda (stx type-name) (syntax-identifier stx))
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
     (fprintf port "~a" (node-identifier self)))]
  #:methods  gen:equal+hash
  [(define (equal-proc s0 s1 equal?)
     (let ([s0 (node-identifier s0)]
           [s1 (node-identifier s1)])
       (and (equal? (syntax-source s0) (syntax-source s1))
            (equal? (syntax-position s0) (syntax-position s1))
            (equal? (syntax-span s0) (syntax-span s1))
            (equal? (syntax-line s0) (syntax-line s1))
            (equal? (syntax-column s0) (syntax-column s1)))))      
   (define (hash-proc s0 hash) 
     (let ([s0 (node-identifier s0)])
       (hash (list (syntax-source s0) (syntax-position s0) 
                   (syntax-span s0) (syntax-line s0) (syntax-column s0)))))
   (define (hash2-proc s0 hash)
     (hash-proc s0 hash))])


