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
; Record 8 fields
(define-symbolic r8f0zpath tag? #:length max_zpath_depth)
(define-symbolic r8f1zpath tag? #:length max_zpath_depth)

(define-symbolic r8fieldmask boolean? #:length max_zpath_depth)
; Record 9 fields
(define-symbolic r9f0zpath tag? #:length max_zpath_depth)
(define-symbolic r9f1zpath tag? #:length max_zpath_depth)

(define-symbolic r9fieldmask boolean? #:length max_zpath_depth)
; Record 10 fields
(define-symbolic r10f0zpath tag? #:length max_zpath_depth)
(define-symbolic r10f1zpath tag? #:length max_zpath_depth)

(define-symbolic r10fieldmask boolean? #:length max_zpath_depth)
; Record 11 fields
(define-symbolic r11f0zpath tag? #:length max_zpath_depth)
(define-symbolic r11f1zpath tag? #:length max_zpath_depth)

(define-symbolic r11fieldmask boolean? #:length max_zpath_depth)
; Record 12 fields
(define-symbolic r12f0zpath tag? #:length max_zpath_depth)
(define-symbolic r12f1zpath tag? #:length max_zpath_depth)

(define-symbolic r12fieldmask boolean? #:length max_zpath_depth)
; Record 13 fields
(define-symbolic r13f0zpath tag? #:length max_zpath_depth)
(define-symbolic r13f1zpath tag? #:length max_zpath_depth)

(define-symbolic r13fieldmask boolean? #:length max_zpath_depth)
; Record 14 fields
(define-symbolic r14f0zpath tag? #:length max_zpath_depth)
(define-symbolic r14f1zpath tag? #:length max_zpath_depth)

(define-symbolic r14fieldmask boolean? #:length max_zpath_depth)
; Record 15 fields
(define-symbolic r15f0zpath tag? #:length max_zpath_depth)
(define-symbolic r15f1zpath tag? #:length max_zpath_depth)

(define-symbolic r15fieldmask boolean? #:length max_zpath_depth)

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

	; Record 8 zpath asserts
	(assert (path? r8f0zpath dom "Somebody's Heartbreak"))
	(assert (path? r8f1zpath dom "Hunter Hayes"))

	; Record 9 zpath asserts
	(assert (path? r9f0zpath dom "A Thousand Years"))
	(assert (path? r9f1zpath dom "Christina Perri"))

	; Record 10 zpath asserts
	(assert (path? r10f0zpath dom "Wanted"))
	(assert (path? r10f1zpath dom "Hunter Hayes"))

	; Record 11 zpath asserts
	(assert (path? r11f0zpath dom "Round Here"))
	(assert (path? r11f1zpath dom "Florida Georgia Line"))

	; Record 12 zpath asserts
	(assert (path? r12f0zpath dom "Little Talks"))
	(assert (path? r12f1zpath dom "Of Monsters and Men"))

	; Record 13 zpath asserts
	(assert (path? r13f0zpath dom "Catch My Breath"))
	(assert (path? r13f1zpath dom "Kelly Clarkson"))

	; Record 14 zpath asserts
	(assert (path? r14f0zpath dom "Feel Again"))
	(assert (path? r14f1zpath dom "OneRepublic"))

	; Record 15 zpath asserts
	(assert (path? r15f0zpath dom "It's Time"))
	(assert (path? r15f1zpath dom "Imagine Dragons"))

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

	; Record 8 Field Mask Generation
	(generate-mask r8f0zpath r8f1zpath r8fieldmask max_zpath_depth)

	; Record 9 Field Mask Generation
	(generate-mask r9f0zpath r9f1zpath r9fieldmask max_zpath_depth)

	; Record 10 Field Mask Generation
	(generate-mask r10f0zpath r10f1zpath r10fieldmask max_zpath_depth)

	; Record 11 Field Mask Generation
	(generate-mask r11f0zpath r11f1zpath r11fieldmask max_zpath_depth)

	; Record 12 Field Mask Generation
	(generate-mask r12f0zpath r12f1zpath r12fieldmask max_zpath_depth)

	; Record 13 Field Mask Generation
	(generate-mask r13f0zpath r13f1zpath r13fieldmask max_zpath_depth)

	; Record 14 Field Mask Generation
	(generate-mask r14f0zpath r14f1zpath r14fieldmask max_zpath_depth)

	; Record 15 Field Mask Generation
	(generate-mask r15f0zpath r15f1zpath r15fieldmask max_zpath_depth)


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

	(zip 
		(DOM-Flatten (DOM-XPath dom field0_zpath))
		(DOM-Flatten (DOM-XPath dom field1_zpath))
	))

(scrape)
