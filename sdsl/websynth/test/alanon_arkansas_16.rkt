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
; Record 8 fields
(define-symbolic r8f0zpath tag? #:length max_zpath_depth)
(define-symbolic r8f1zpath tag? #:length max_zpath_depth)
(define-symbolic r8f2zpath tag? #:length max_zpath_depth)

(define-symbolic r8fieldmask boolean? #:length max_zpath_depth)
; Record 9 fields
(define-symbolic r9f0zpath tag? #:length max_zpath_depth)
(define-symbolic r9f1zpath tag? #:length max_zpath_depth)
(define-symbolic r9f2zpath tag? #:length max_zpath_depth)

(define-symbolic r9fieldmask boolean? #:length max_zpath_depth)
; Record 10 fields
(define-symbolic r10f0zpath tag? #:length max_zpath_depth)
(define-symbolic r10f1zpath tag? #:length max_zpath_depth)
(define-symbolic r10f2zpath tag? #:length max_zpath_depth)

(define-symbolic r10fieldmask boolean? #:length max_zpath_depth)
; Record 11 fields
(define-symbolic r11f0zpath tag? #:length max_zpath_depth)
(define-symbolic r11f1zpath tag? #:length max_zpath_depth)
(define-symbolic r11f2zpath tag? #:length max_zpath_depth)

(define-symbolic r11fieldmask boolean? #:length max_zpath_depth)
; Record 12 fields
(define-symbolic r12f0zpath tag? #:length max_zpath_depth)
(define-symbolic r12f1zpath tag? #:length max_zpath_depth)
(define-symbolic r12f2zpath tag? #:length max_zpath_depth)

(define-symbolic r12fieldmask boolean? #:length max_zpath_depth)
; Record 13 fields
(define-symbolic r13f0zpath tag? #:length max_zpath_depth)
(define-symbolic r13f1zpath tag? #:length max_zpath_depth)
(define-symbolic r13f2zpath tag? #:length max_zpath_depth)

(define-symbolic r13fieldmask boolean? #:length max_zpath_depth)
; Record 14 fields
(define-symbolic r14f0zpath tag? #:length max_zpath_depth)
(define-symbolic r14f1zpath tag? #:length max_zpath_depth)
(define-symbolic r14f2zpath tag? #:length max_zpath_depth)

(define-symbolic r14fieldmask boolean? #:length max_zpath_depth)
; Record 15 fields
(define-symbolic r15f0zpath tag? #:length max_zpath_depth)
(define-symbolic r15f1zpath tag? #:length max_zpath_depth)
(define-symbolic r15f2zpath tag? #:length max_zpath_depth)

(define-symbolic r15fieldmask boolean? #:length max_zpath_depth)

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
	(assert (path? r6f0zpath dom "ODAT AFG"))
	(assert (path? r6f1zpath dom "2701 Old Greenwood Rd"))
	(assert (path? r6f2zpath dom "Fort Smith"))

	; Record 7 zpath asserts
	(assert (path? r7f0zpath dom "Amor Fe Y Esperanza GFA"))
	(assert (path? r7f1zpath dom "1506 W Robinson Ave"))
	(assert (path? r7f2zpath dom "Springdale"))

	; Record 8 zpath asserts
	(assert (path? r8f0zpath dom "One Day At A Time AFG"))
	(assert (path? r8f1zpath dom "275 Asturias Drive"))
	(assert (path? r8f2zpath dom "Hot Springs Village"))

	; Record 9 zpath asserts
	(assert (path? r9f0zpath dom "Wynne Tuesday Night AFG"))
	(assert (path? r9f1zpath dom "1201 South Falls Blvd"))
	(assert (path? r9f2zpath dom "Wynne"))

	; Record 10 zpath asserts
	(assert (path? r10f0zpath dom "9:45AM Coffee Break Womens Meeting"))
	(assert (path? r10f1zpath dom "157 Huntsville Rd Hwy 23 S"))
	(assert (path? r10f2zpath dom "Eureka Springs"))

	; Record 11 zpath asserts
	(assert (path? r11f0zpath dom "Pine Bluff AFG"))
	(assert (path? r11f1zpath dom "4101 Hazel Street"))
	(assert (path? r11f2zpath dom "Pine Bluff"))

	; Record 12 zpath asserts
	(assert (path? r12f0zpath dom "Quapaw Noon AFG"))
	(assert (path? r12f1zpath dom "300 W 17th St"))
	(assert (path? r12f2zpath dom "Little Rock"))

	; Record 13 zpath asserts
	(assert (path? r13f0zpath dom "River Valley AFG"))
	(assert (path? r13f1zpath dom "111 Quay St"))
	(assert (path? r13f2zpath dom "Dardanelle"))

	; Record 14 zpath asserts
	(assert (path? r14f0zpath dom "Monday Serenity AFG"))
	(assert (path? r14f1zpath dom "#1 St. Bernard Lane"))
	(assert (path? r14f2zpath dom "Bella Vista"))

	; Record 15 zpath asserts
	(assert (path? r15f0zpath dom "Batesville AFG"))
	(assert (path? r15f1zpath dom "7th and Water Street"))
	(assert (path? r15f2zpath dom "Batesville"))

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

	; Record 8 Field Mask Generation
	(generate-mask r8f0zpath r8f1zpath r8fieldmask max_zpath_depth)
	(generate-mask r8f1zpath r8f2zpath r8fieldmask max_zpath_depth)

	; Record 9 Field Mask Generation
	(generate-mask r9f0zpath r9f1zpath r9fieldmask max_zpath_depth)
	(generate-mask r9f1zpath r9f2zpath r9fieldmask max_zpath_depth)

	; Record 10 Field Mask Generation
	(generate-mask r10f0zpath r10f1zpath r10fieldmask max_zpath_depth)
	(generate-mask r10f1zpath r10f2zpath r10fieldmask max_zpath_depth)

	; Record 11 Field Mask Generation
	(generate-mask r11f0zpath r11f1zpath r11fieldmask max_zpath_depth)
	(generate-mask r11f1zpath r11f2zpath r11fieldmask max_zpath_depth)

	; Record 12 Field Mask Generation
	(generate-mask r12f0zpath r12f1zpath r12fieldmask max_zpath_depth)
	(generate-mask r12f1zpath r12f2zpath r12fieldmask max_zpath_depth)

	; Record 13 Field Mask Generation
	(generate-mask r13f0zpath r13f1zpath r13fieldmask max_zpath_depth)
	(generate-mask r13f1zpath r13f2zpath r13fieldmask max_zpath_depth)

	; Record 14 Field Mask Generation
	(generate-mask r14f0zpath r14f1zpath r14fieldmask max_zpath_depth)
	(generate-mask r14f1zpath r14f2zpath r14fieldmask max_zpath_depth)

	; Record 15 Field Mask Generation
	(generate-mask r15f0zpath r15f1zpath r15fieldmask max_zpath_depth)
	(generate-mask r15f1zpath r15f2zpath r15fieldmask max_zpath_depth)


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
	; Record 8 zpaths
	; Record 9 zpaths
	; Record 10 zpaths
	; Record 11 zpaths
	; Record 12 zpaths
	; Record 13 zpaths
	; Record 14 zpaths
	; Record 15 zpaths

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

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(define-runtime-path out (build-path "." "alanon_arkansas.out"))

(define a-test
	(test-suite+ 
		"alanon_arkansas_16"
		(test-case "alanon_arkansas_16"
			(define expected (second (call-with-input-file out read)))
			(define actual (scrape))
			(check-equal? actual expected))))
(time (run-tests a-test))
