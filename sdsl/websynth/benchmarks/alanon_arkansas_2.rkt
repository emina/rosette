#lang rosette

(require (only-in racket/runtime-path define-runtime-path))
(require "../dom.rkt")
(require "../websynth.rkt")
(require "../websynthlib.rkt")

(define-runtime-path html (build-path ".." "html/alanon_arkansas.html"))
(define dom (read-DOMNode html))
(define-tags (tags dom))
(define max_zpath_depth (depth dom))

; Record 0 fields
(define-symbolic r0f0zpath tag? #:length max_zpath_depth)
(define-symbolic r0f1zpath tag? #:length max_zpath_depth)
(define-symbolic r0f2zpath tag? #:length max_zpath_depth)

(define-symbolic r0fieldmask boolean? #:length max_zpath_depth)
; Record 1 fields
(define-symbolic r1f0zpath tag? #:length max_zpath_depth)
(define-symbolic r1f1zpath tag? #:length max_zpath_depth)
(define-symbolic r1f2zpath tag? #:length max_zpath_depth)

(define-symbolic r1fieldmask boolean? #:length max_zpath_depth)

; Cross-record Mask
(define-symbolic recordmask boolean? #:length max_zpath_depth)
(current-bitwidth #f)

(define (demonstration)

	; Record 0 zpath asserts
	(assert (path? r0f0zpath dom "Alexander AFG"))
	(assert (path? r0f1zpath dom "15224 Alexander Road"))
	(assert (path? r0f2zpath dom "Alexander"))

	; Record 1 zpath asserts
	(assert (path? r1f0zpath dom "Circle Of Hope AFG"))
	(assert (path? r1f1zpath dom "1606 N Franklin St"))
	(assert (path? r1f2zpath dom "Altus"))

	; Record 0 Field Mask Generation
	(generate-mask r0f0zpath r0f1zpath r0fieldmask max_zpath_depth)
	(generate-mask r0f1zpath r0f2zpath r0fieldmask max_zpath_depth)

	; Record 1 Field Mask Generation
	(generate-mask r1f0zpath r1f1zpath r1fieldmask max_zpath_depth)
	(generate-mask r1f1zpath r1f2zpath r1fieldmask max_zpath_depth)


	; Record Mask
	(generate-mask r0f0zpath r1f0zpath recordmask max_zpath_depth))

; Solve
(define (scrape)
	(define sol (solve (demonstration)))

	; Record 0 zpaths
	; Record 1 zpaths

	; Construct final zpaths
	(define r0f0zpath_list (map label (evaluate r0f0zpath sol)))
	(define generalized_r0f0zpath_list 
		(apply-mask r0f0zpath_list (evaluate recordmask sol)))
	(define field0_zpath (synthsis_solution->zpath generalized_r0f0zpath_list))

	(define r0f1zpath_list (map label (evaluate r0f1zpath sol)))
	(define generalized_r0f1zpath_list 
		(apply-mask r0f1zpath_list (evaluate recordmask sol)))
	(define field1_zpath (synthsis_solution->zpath generalized_r0f1zpath_list))

	(define r0f2zpath_list (map label (evaluate r0f2zpath sol)))
	(define generalized_r0f2zpath_list 
		(apply-mask r0f2zpath_list (evaluate recordmask sol)))
	(define field2_zpath (synthsis_solution->zpath generalized_r0f2zpath_list))

	(zip 
		(DOM-Flatten (DOM-XPath dom field0_zpath))
		(DOM-Flatten (DOM-XPath dom field1_zpath))
		(DOM-Flatten (DOM-XPath dom field2_zpath))
	))

(scrape)
