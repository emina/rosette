#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit "../model/work.rkt") 

(define work-tests
  (test-suite+ 
   "Tests for work item functions"
   (parameterize ([current-work-size '(()(1)())]
                  [current-global-id 0])
     (check-equal? (get_global_size 0) 1)
     (check-equal? (get_local_size 0) 1)
     (check-equal? (get_num_groups 0) 1)
     (check-equal? (get_global_offset 0) 0)
     (check-equal? (get_global_id 0) 0)
     (check-equal? (get_local_id 0) 0)
     (check-equal? (get_group_id 0) 0)
     (current-work-size '((1)(4)(2)))
     (check-equal? (get_global_size 0) 4)
     (check-equal? (get_local_size 0) 2)
     (check-equal? (get_num_groups 0) 2)
     (check-equal? (get_global_offset 0) 1)
     (check-equal? (get_global_id 0) 0)
     (check-equal? (get_local_id 0) 0)
     (check-equal? (get_group_id 0) 0)
     (current-global-id '(4))
     (check-equal? (get_global_id 0) 4)
     (check-equal? (get_local_id 0) 1)
     (check-equal? (get_group_id 0) 1))))

(time (run-tests work-tests))

#|
(define-symbolic off local number?)
(current-work-size (list (list off) (list 4) (list local)))
(get_global_offset 0)
(current-global-id 3)
(get_global_id 0)|#
