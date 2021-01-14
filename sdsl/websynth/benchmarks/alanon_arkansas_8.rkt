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
; Record 2 fields
(define-symbolic r2f0zpath tag? #:length max_zpath_depth)
(define-symbolic r2f1zpath tag? #:length max_zpath_depth)
(define-symbolic r2f2zpath tag? #:length max_zpath_depth)

(define-symbolic r2fieldmask boolean? #:length max_zpath_depth)
; Record 3 fields
(define-symbolic r3f0zpath tag? #:length max_zpath_depth)
(define-symbolic r3f1zpath tag? #:length max_zpath_depth)
(define-symbolic r3f2zpath tag? #:length max_zpath_depth)

(define-symbolic r3fieldmask boolean? #:length max_zpath_depth)
; Record 4 fields
(define-symbolic r4f0zpath tag? #:length max_zpath_depth)
(define-symbolic r4f1zpath tag? #:length max_zpath_depth)
(define-symbolic r4f2zpath tag? #:length max_zpath_depth)

(define-symbolic r4fieldmask boolean? #:length max_zpath_depth)
; Record 5 fields
(define-symbolic r5f0zpath tag? #:length max_zpath_depth)
(define-symbolic r5f1zpath tag? #:length max_zpath_depth)
(define-symbolic r5f2zpath tag? #:length max_zpath_depth)

(define-symbolic r5fieldmask boolean? #:length max_zpath_depth)
; Record 6 fields
(define-symbolic r6f0zpath tag? #:length max_zpath_depth)
(define-symbolic r6f1zpath tag? #:length max_zpath_depth)
(define-symbolic r6f2zpath tag? #:length max_zpath_depth)

(define-symbolic r6fieldmask boolean? #:length max_zpath_depth)
; Record 7 fields
(define-symbolic r7f0zpath tag? #:length max_zpath_depth)
(define-symbolic r7f1zpath tag? #:length max_zpath_depth)
(define-symbolic r7f2zpath tag? #:length max_zpath_depth)

(define-symbolic r7fieldmask boolean? #:length max_zpath_depth)

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

	; Record 2 zpath asserts
	(assert (path? r2f0zpath dom "Primary Purpose Adult Children AFG"))
	(assert (path? r2f1zpath dom "1922 Dodson"))
	(assert (path? r2f2zpath dom "Fort Smith"))

	; Record 3 zpath asserts
	(assert (path? r3f0zpath dom "Cosmo Happy Hour AFG"))
	(assert (path? r3f1zpath dom "4724 Hillcrest Ave"))
	(assert (path? r3f2zpath dom "Little Rock"))

	; Record 4 zpath asserts
	(assert (path? r4f0zpath dom "Rose City AFG"))
	(assert (path? r4f1zpath dom "4525 Lynch Drive"))
	(assert (path? r4f2zpath dom "North Little Rock"))

	; Record 5 zpath asserts
	(assert (path? r5f0zpath dom "Cabot Recovery AFG"))
	(assert (path? r5f1zpath dom "301 S. Pine Street"))
	(assert (path? r5f2zpath dom "Cabot"))

	; Record 6 zpath asserts
	(assert (path? r6f0zpath dom "One Day At A Time AFG"))
	(assert (path? r6f1zpath dom "275 Asturias Drive"))
	(assert (path? r6f2zpath dom "Hot Springs Village"))

	; Record 7 zpath asserts
	(assert (path? r7f0zpath dom "Wynne Tuesday Night AFG"))
	(assert (path? r7f1zpath dom "1201 South Falls Blvd"))
	(assert (path? r7f2zpath dom "Wynne"))

	; Record 0 Field Mask Generation
	(generate-mask r0f0zpath r0f1zpath r0fieldmask max_zpath_depth)
	(generate-mask r0f1zpath r0f2zpath r0fieldmask max_zpath_depth)

	; Record 1 Field Mask Generation
	(generate-mask r1f0zpath r1f1zpath r1fieldmask max_zpath_depth)
	(generate-mask r1f1zpath r1f2zpath r1fieldmask max_zpath_depth)

	; Record 2 Field Mask Generation
	(generate-mask r2f0zpath r2f1zpath r2fieldmask max_zpath_depth)
	(generate-mask r2f1zpath r2f2zpath r2fieldmask max_zpath_depth)

	; Record 3 Field Mask Generation
	(generate-mask r3f0zpath r3f1zpath r3fieldmask max_zpath_depth)
	(generate-mask r3f1zpath r3f2zpath r3fieldmask max_zpath_depth)

	; Record 4 Field Mask Generation
	(generate-mask r4f0zpath r4f1zpath r4fieldmask max_zpath_depth)
	(generate-mask r4f1zpath r4f2zpath r4fieldmask max_zpath_depth)

	; Record 5 Field Mask Generation
	(generate-mask r5f0zpath r5f1zpath r5fieldmask max_zpath_depth)
	(generate-mask r5f1zpath r5f2zpath r5fieldmask max_zpath_depth)

	; Record 6 Field Mask Generation
	(generate-mask r6f0zpath r6f1zpath r6fieldmask max_zpath_depth)
	(generate-mask r6f1zpath r6f2zpath r6fieldmask max_zpath_depth)

	; Record 7 Field Mask Generation
	(generate-mask r7f0zpath r7f1zpath r7fieldmask max_zpath_depth)
	(generate-mask r7f1zpath r7f2zpath r7fieldmask max_zpath_depth)


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
