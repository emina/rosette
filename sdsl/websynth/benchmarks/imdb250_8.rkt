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

	(zip 
		(DOM-Flatten (DOM-XPath dom field0_zpath))
	))

(scrape)
