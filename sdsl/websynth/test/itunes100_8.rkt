#lang rosette

(require (only-in racket/runtime-path define-runtime-path))
(require "../dom.rkt")
(require "../websynth.rkt")
(require "../websynthlib.rkt")

(define-runtime-path html (build-path ".." "html/itunes_top100_v2.html"))
(define dom (read-DOMNode html))
(define-tags (tags dom))
(define max_zpath_depth (depth dom))

; Record 0 fields
(define-symbolic r0f0zpath tag? #:length max_zpath_depth)
(define-symbolic r0f1zpath tag? #:length max_zpath_depth)

(define-symbolic r0fieldmask boolean? #:length max_zpath_depth)
; Record 1 fields
(define-symbolic r1f0zpath tag? #:length max_zpath_depth)
(define-symbolic r1f1zpath tag? #:length max_zpath_depth)

(define-symbolic r1fieldmask boolean? #:length max_zpath_depth)
; Record 2 fields
(define-symbolic r2f0zpath tag? #:length max_zpath_depth)
(define-symbolic r2f1zpath tag? #:length max_zpath_depth)

(define-symbolic r2fieldmask boolean? #:length max_zpath_depth)
; Record 3 fields
(define-symbolic r3f0zpath tag? #:length max_zpath_depth)
(define-symbolic r3f1zpath tag? #:length max_zpath_depth)

(define-symbolic r3fieldmask boolean? #:length max_zpath_depth)
; Record 4 fields
(define-symbolic r4f0zpath tag? #:length max_zpath_depth)
(define-symbolic r4f1zpath tag? #:length max_zpath_depth)

(define-symbolic r4fieldmask boolean? #:length max_zpath_depth)
; Record 5 fields
(define-symbolic r5f0zpath tag? #:length max_zpath_depth)
(define-symbolic r5f1zpath tag? #:length max_zpath_depth)

(define-symbolic r5fieldmask boolean? #:length max_zpath_depth)
; Record 6 fields
(define-symbolic r6f0zpath tag? #:length max_zpath_depth)
(define-symbolic r6f1zpath tag? #:length max_zpath_depth)

(define-symbolic r6fieldmask boolean? #:length max_zpath_depth)
; Record 7 fields
(define-symbolic r7f0zpath tag? #:length max_zpath_depth)
(define-symbolic r7f1zpath tag? #:length max_zpath_depth)

(define-symbolic r7fieldmask boolean? #:length max_zpath_depth)

; Cross-record Mask
(define-symbolic recordmask boolean? #:length max_zpath_depth)
(current-bitwidth #f)

(define (demonstration)

	; Record 0 zpath asserts
	(assert (path? r0f0zpath dom "Sail"))
	(assert (path? r0f1zpath dom "AWOLNATION"))

	; Record 1 zpath asserts
	(assert (path? r1f0zpath dom "I Won't Give Up"))
	(assert (path? r1f1zpath dom "Jason Mraz"))

	; Record 2 zpath asserts
	(assert (path? r2f0zpath dom "Diamonds"))
	(assert (path? r2f1zpath dom "Rihanna"))

	; Record 3 zpath asserts
	(assert (path? r3f0zpath dom "What Christmas Means to Me"))
	(assert (path? r3f1zpath dom "Cee Lo Green"))

	; Record 4 zpath asserts
	(assert (path? r4f0zpath dom "Til My Last Day"))
	(assert (path? r4f1zpath dom "Justin Moore"))

	; Record 5 zpath asserts
	(assert (path? r5f0zpath dom "Girl On Fire"))
	(assert (path? r5f1zpath dom "Alicia Keys"))

	; Record 6 zpath asserts
	(assert (path? r6f0zpath dom "Little Talks"))
	(assert (path? r6f1zpath dom "Of Monsters and Men"))

	; Record 7 zpath asserts
	(assert (path? r7f0zpath dom "When I Was Your Man"))
	(assert (path? r7f1zpath dom "Bruno Mars"))

	; Record 0 Field Mask Generation
	(generate-mask r0f0zpath r0f1zpath r0fieldmask max_zpath_depth)

	; Record 1 Field Mask Generation
	(generate-mask r1f0zpath r1f1zpath r1fieldmask max_zpath_depth)

	; Record 2 Field Mask Generation
	(generate-mask r2f0zpath r2f1zpath r2fieldmask max_zpath_depth)

	; Record 3 Field Mask Generation
	(generate-mask r3f0zpath r3f1zpath r3fieldmask max_zpath_depth)

	; Record 4 Field Mask Generation
	(generate-mask r4f0zpath r4f1zpath r4fieldmask max_zpath_depth)

	; Record 5 Field Mask Generation
	(generate-mask r5f0zpath r5f1zpath r5fieldmask max_zpath_depth)

	; Record 6 Field Mask Generation
	(generate-mask r6f0zpath r6f1zpath r6fieldmask max_zpath_depth)

	; Record 7 Field Mask Generation
	(generate-mask r7f0zpath r7f1zpath r7fieldmask max_zpath_depth)


	; Record Mask
	(generate-mask r0f0zpath r1f0zpath recordmask max_zpath_depth))

; Solve
(define (scrape)
	(define sol (solve (demonstration)))

	; Record 0 zpaths
	; Record 1 zpaths
	; Record 2 zpaths
	; Record 3 zpaths
	; Record 4 zpaths
	; Record 5 zpaths
	; Record 6 zpaths
	; Record 7 zpaths

	; Construct final zpaths
	(define r0f0zpath_list (map label (evaluate r0f0zpath sol)))
	(define generalized_r0f0zpath_list 
		(apply-mask r0f0zpath_list (evaluate recordmask sol)))
	(define field0_zpath (synthsis_solution->zpath generalized_r0f0zpath_list))

	(define r0f1zpath_list (map label (evaluate r0f1zpath sol)))
	(define generalized_r0f1zpath_list 
		(apply-mask r0f1zpath_list (evaluate recordmask sol)))
	(define field1_zpath (synthsis_solution->zpath generalized_r0f1zpath_list))

	(zip 
		(DOM-Flatten (DOM-XPath dom field0_zpath))
		(DOM-Flatten (DOM-XPath dom field1_zpath))
	))

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(define-runtime-path out (build-path "." "itunes_top100_v2.out"))

(define a-test
	(test-suite+ 
		"itunes100_8"
		(test-case "itunes100_8"
			(define expected (second (call-with-input-file out read)))
			(define actual (scrape))
			(check-equal? actual expected))))
(time (run-tests a-test))
