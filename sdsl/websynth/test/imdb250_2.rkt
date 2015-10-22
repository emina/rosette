#lang s-exp rosette

(require (only-in racket/runtime-path define-runtime-path))
(require "../dom.rkt")
(require "../websynth.rkt")
(require "../websynthlib.rkt")

(define-runtime-path html (build-path ".." "html/imdb250.html"))
(define dom (read-DOMNode html))
(define-tags (tags dom))
(define max_zpath_depth (depth dom))

; Record 0 fields
(define-symbolic r0f0zpath tag? [max_zpath_depth])

(define-symbolic r0fieldmask boolean? [max_zpath_depth])
; Record 1 fields
(define-symbolic r1f0zpath tag? [max_zpath_depth])

(define-symbolic r1fieldmask boolean? [max_zpath_depth])

; Cross-record Mask
(define-symbolic recordmask boolean? [max_zpath_depth])
(current-bitwidth 1)

(define (demonstration)

	; Record 0 zpath asserts
	(assert (path? r0f0zpath dom "The Shawshank Redemption"))

	; Record 1 zpath asserts
	(assert (path? r1f0zpath dom "Fight Club"))

	; Record Mask
	(generate-mask r0f0zpath r1f0zpath recordmask max_zpath_depth))

; Solve
(define (scrape)
	(define sol (solve (demonstration)))

	; Record 0 zpaths
	; Record 1 zpaths

	; Construct final zpaths
	(define r0f0zpath_list (map label (evaluate r0f0zpath)))
	(define generalizelized_r0f0zpath_list 
		(apply-mask r0f0zpath_list (evaluate recordmask)))
	(define field0_zpath (synthsis_solution->zpath generalizelized_r0f0zpath_list))

	(zip 
		(DOM-Flatten (DOM-XPath dom field0_zpath))
	))

(require rackunit rackunit/text-ui)
(define-runtime-path out (build-path "." "imdb250.out"))

(define a-test
	(test-suite 
		"imdb250_2"
		#:before (lambda () (printf "Testing imdb250_2.~n"))
		(test-case "imdb250_2"
			(current-solution (empty-solution))
			(clear-asserts)
			(unsafe-clear-terms!)
			(define expected (second (call-with-input-file out read)))
			(define actual (scrape))
			(check-equal? actual expected))))

(time (run-tests a-test))
