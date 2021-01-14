#lang rosette

(require (only-in racket/runtime-path define-runtime-path))
(require "../dom.rkt")
(require "../websynth.rkt")
(require "../websynthlib.rkt")

(define-runtime-path html (build-path ".." "html/imdb250.html"))
(define dom (read-DOMNode html))
(define-tags (tags dom))
(define max_zpath_depth (depth dom))

; Record 0 fields
(define-symbolic r0f0zpath tag? #:length max_zpath_depth)

(define-symbolic r0fieldmask boolean? #:length max_zpath_depth)
; Record 1 fields
(define-symbolic r1f0zpath tag? #:length max_zpath_depth)

(define-symbolic r1fieldmask boolean? #:length max_zpath_depth)
; Record 2 fields
(define-symbolic r2f0zpath tag? #:length max_zpath_depth)

(define-symbolic r2fieldmask boolean? #:length max_zpath_depth)
; Record 3 fields
(define-symbolic r3f0zpath tag? #:length max_zpath_depth)

(define-symbolic r3fieldmask boolean? #:length max_zpath_depth)
; Record 4 fields
(define-symbolic r4f0zpath tag? #:length max_zpath_depth)

(define-symbolic r4fieldmask boolean? #:length max_zpath_depth)
; Record 5 fields
(define-symbolic r5f0zpath tag? #:length max_zpath_depth)

(define-symbolic r5fieldmask boolean? #:length max_zpath_depth)
; Record 6 fields
(define-symbolic r6f0zpath tag? #:length max_zpath_depth)

(define-symbolic r6fieldmask boolean? #:length max_zpath_depth)
; Record 7 fields
(define-symbolic r7f0zpath tag? #:length max_zpath_depth)

(define-symbolic r7fieldmask boolean? #:length max_zpath_depth)
; Record 8 fields
(define-symbolic r8f0zpath tag? #:length max_zpath_depth)

(define-symbolic r8fieldmask boolean? #:length max_zpath_depth)
; Record 9 fields
(define-symbolic r9f0zpath tag? #:length max_zpath_depth)

(define-symbolic r9fieldmask boolean? #:length max_zpath_depth)
; Record 10 fields
(define-symbolic r10f0zpath tag? #:length max_zpath_depth)

(define-symbolic r10fieldmask boolean? #:length max_zpath_depth)
; Record 11 fields
(define-symbolic r11f0zpath tag? #:length max_zpath_depth)

(define-symbolic r11fieldmask boolean? #:length max_zpath_depth)
; Record 12 fields
(define-symbolic r12f0zpath tag? #:length max_zpath_depth)

(define-symbolic r12fieldmask boolean? #:length max_zpath_depth)
; Record 13 fields
(define-symbolic r13f0zpath tag? #:length max_zpath_depth)

(define-symbolic r13fieldmask boolean? #:length max_zpath_depth)
; Record 14 fields
(define-symbolic r14f0zpath tag? #:length max_zpath_depth)

(define-symbolic r14fieldmask boolean? #:length max_zpath_depth)
; Record 15 fields
(define-symbolic r15f0zpath tag? #:length max_zpath_depth)

(define-symbolic r15fieldmask boolean? #:length max_zpath_depth)

; Cross-record Mask
(define-symbolic recordmask boolean? #:length max_zpath_depth)
(current-bitwidth #f)

(define (demonstration)

	; Record 0 zpath asserts
	(assert (path? r0f0zpath dom "The Shawshank Redemption"))

	; Record 1 zpath asserts
	(assert (path? r1f0zpath dom "Fight Club"))

	; Record 2 zpath asserts
	(assert (path? r2f0zpath dom "The Big Sleep"))

	; Record 3 zpath asserts
	(assert (path? r3f0zpath dom "In the Mood for Love"))

	; Record 4 zpath asserts
	(assert (path? r4f0zpath dom "The Celebration"))

	; Record 5 zpath asserts
	(assert (path? r5f0zpath dom "The Untouchables"))

	; Record 6 zpath asserts
	(assert (path? r6f0zpath dom "The Bourne Ultimatum"))

	; Record 7 zpath asserts
	(assert (path? r7f0zpath dom "Requiem for a Dream"))

	; Record 8 zpath asserts
	(assert (path? r8f0zpath dom "Das Boot"))

	; Record 9 zpath asserts
	(assert (path? r9f0zpath dom "The Lives of Others"))

	; Record 10 zpath asserts
	(assert (path? r10f0zpath dom "Singin' in the Rain"))

	; Record 11 zpath asserts
	(assert (path? r11f0zpath dom "The Hunt"))

	; Record 12 zpath asserts
	(assert (path? r12f0zpath dom "The Wild Bunch"))

	; Record 13 zpath asserts
	(assert (path? r13f0zpath dom "Rain Man"))

	; Record 14 zpath asserts
	(assert (path? r14f0zpath dom "Seven Samurai"))

	; Record 15 zpath asserts
	(assert (path? r15f0zpath dom "Amadeus"))

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

	(zip 
		(DOM-Flatten (DOM-XPath dom field0_zpath))
	))

(scrape)
