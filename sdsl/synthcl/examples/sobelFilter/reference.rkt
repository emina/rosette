#lang s-exp "../../lang/main.rkt"

; Gradient masks for x and y axes.
(: int kx00 kx01 kx02 kx10 kx11 kx12 kx20 kx21 kx22
       ky00 ky01 ky02 ky10 ky11 ky12 ky20 ky21 ky22)

(= kx00  1)  (= kx01  2)  (= kx02  1)  ; [ 1  2  1
(= kx10  0)  (= kx11  0)  (= kx12  0)  ;   0  0  0
(= kx20 -1)  (= kx21 -2)  (= kx22 -1)  ;  -1 -2 -1 ]

(= ky00  1)  (= ky01  0)  (= ky02 -1)  ; [ 1  0 -1
(= ky10  2)  (= ky11  0)  (= ky12 -2)  ;   2  0 -2
(= ky20  1)  (= ky21  0)  (= ky22 -1)  ;   1  0 -1 ]

; A transcription of Samsung's reference implementation of the Sobel filter, in 
; which we represent images as arrays of ints rather than chars. The reference 
; implementation applies the Sobel filter to a width x height x pixelSize image,
; represented as a flat array of ints, where pixelSize = 4 * (sizeof int).
(procedure int* (sobelFilter0 [int* inputImage] [int width] [int height] [int pixelSize])
           
  ; Allocate the output array, and set all of its elements to 0.         
  (: int* outputImage)
  (= outputImage ((int*) (malloc (* width height pixelSize))))
  (memset outputImage 0 (* width height pixelSize))
  
  (: int gx gy k w)
  (= w (* width 4)) ; implicit assumption:  pixelSize = 4 * (sizeof pixelElementType)
  (= k 1)
  
  (for [(: int i in (range ((int) (* w (- height 1)))))]
    (if (&& (< i (* (+ k 1) w)) (>= i (+ 4 (* k w))))
        { (= gx (+ (* kx00 [inputImage (- i 4 w)])     (* kx01 [inputImage (- i w)]) (* kx02 [inputImage (- (+ i 4) w)])
                   (* kx10 [inputImage (- i 4)])       (* kx11 [inputImage i])       (* kx12 [inputImage (+ i 4)])
                   (* kx20 [inputImage (+ (- i 4) w)]) (* kx21 [inputImage (+ i w)]) (* kx22 [inputImage (+ i 4 w)])))
          (= gy (+ (* ky00 [inputImage (- i 4 w)])     (* ky01 [inputImage (- i w)]) (* ky02 [inputImage (- (+ i 4) w)])
                   (* ky10 [inputImage (- i 4)])       (* ky11 [inputImage i])       (* ky12 [inputImage (+ i 4)])
                   (* ky20 [inputImage (+ (- i 4) w)]) (* ky21 [inputImage (+ i w)]) (* ky22 [inputImage (+ i 4 w)])))
          (: float gx2 gy2)
          (= gx2 (* ((float) gx) gx))
          (= gy2 (* ((float) gy) gy))
          (= [outputImage i] ((int) (/ (sqrt (+ gx2 gy2)) 2))) })
        (if (== i (- (* (+ k 1) w) 5))
            { (+= k 1) }))
  
  outputImage)

; A refinement of sobelFilter0, where we avoid repeated 
; reads of the same locations in the inputImage array to compute
; gx and gy, and we eliminate the temporary variables gx2 and gy2
; (which are used only once).
(procedure int* (sobelFilter1 [int* inputImage] [int width] [int height] [int pixelSize])
                   
  (: int* outputImage)
  (= outputImage ((int*) (malloc (* width height pixelSize))))
  (memset outputImage 0 (* width height pixelSize))
  
  (: int gx gy k w i00 i01 i02 i10 i11 i12 i20 i21 i22)
  (= w (* width 4))
  (= k 1)
  
  (for [(: int i in (range ((int) (* w (- height 1)))))]
    (if (&& (< i (* (+ k 1) w)) (>= i (+ 4 (* k w)))) 
        { (= i00 [inputImage (- i 4 w)])
          (= i01 [inputImage (- i w)])
          (= i02 [inputImage (- (+ i 4) w)])
          (= i10 [inputImage (- i 4)])
          (= i11 [inputImage i])
          (= i12 [inputImage (+ i 4)])
          (= i20 [inputImage (+ (- i 4) w)])
          (= i21 [inputImage (+ i w)])
          (= i22 [inputImage (+ i 4 w)])
          (= gx (+ (* kx00 i00) (* kx01 i01) (* kx02 i02)
                   (* kx10 i10) (* kx11 i11) (* kx12 i12)
                   (* kx20 i20) (* kx21 i21) (* kx22 i22)))
          (= gy (+ (* ky00 i00) (* ky01 i01) (* ky02 i02)
                   (* ky10 i10) (* ky11 i11) (* ky12 i12)
                   (* ky20 i20) (* ky21 i21) (* ky22 i22)))
          (= [outputImage i] ((int) (/ (sqrt (+ (* ((float) gx) gx) (* ((float) gy) gy))) 2))) })
        (if (== i (- (* (+ k 1) w) 5))             
            { (+= k 1) }))
  
  outputImage)

; A refinement of sobelFilter, where we switch from a 1-dimensional iteration 
; space over i to a 2-dimensional iteration space over x and y.
(procedure int* (sobelFilter2 [int* inputImage] [int width] [int height] [int pixelSize])
                  
  (: int* outputImage)
  (= outputImage ((int*) (malloc (* width height pixelSize))))
  (memset outputImage 0 (* width height pixelSize))
  
  (: int gx gy k w i i00 i01 i02 i10 i11 i12 i20 i21 i22)
  (= w (* width 4))
  (= k 1)
  
  (for [(: int y in (range 0 (- height 1)))
        (: int x in (range 0 w))] 
    (= i (+ (* y w) x))
    (if (&& (< i (* (+ k 1) w)) (>= i (+ 4 (* k w)))) ; C0
        { (= i00 [inputImage (- i 4 w)])
          (= i01 [inputImage (- i w)])
          (= i02 [inputImage (- (+ i 4) w)])
          (= i10 [inputImage (- i 4)])
          (= i11 [inputImage i])
          (= i12 [inputImage (+ i 4)])
          (= i20 [inputImage (+ (- i 4) w)])
          (= i21 [inputImage (+ i w)])
          (= i22 [inputImage (+ i 4 w)])
          (= gx (+ (* kx00 i00) (* kx01 i01) (* kx02 i02)
                   (* kx10 i10) (* kx11 i11) (* kx12 i12)
                   (* kx20 i20) (* kx21 i21) (* kx22 i22)))
          (= gy (+ (* ky00 i00) (* ky01 i01) (* ky02 i02)
                   (* ky10 i10) (* ky11 i11) (* ky12 i12)
                   (* ky20 i20) (* ky21 i21) (* ky22 i22)))
          (= [outputImage i] ((int) (/ (sqrt (+ (* ((float) gx) gx) (* ((float) gy) gy))) 2))) }) 
        (if (== i (- (* (+ k 1) w) 5))                ; C1          
            { (+= k 1) }))
  
  outputImage)

; A sketch of a refinement of sobelFilter2, where we get rid of the temporary 
; variable k.  The sketch expresses the hypothesis that the lower and upper bound 
; on i in condition C0 can be replaced with bounds on x and y that are some linear 
; expressions over hieght and w: 
; x bound: (?? * w + ??) and y bound: (?? * height + ??)
(procedure int* (sobelFilter3Sketch [int* inputImage] [int width] [int height] [int pixelSize])
                   
  (: int* outputImage)
  (= outputImage ((int*) (malloc (* width height pixelSize))))
  (memset outputImage 0 (* width height pixelSize))
  
  (: int gx gy w i i00 i01 i02 i10 i11 i12 i20 i21 i22)
  (= w (* width 4))
  
  (for [(: int y in (range 0 (- height 1)))
        (: int x in (range 0 w))] 
    (= i (+ (* y w) x))
    (if (&& (<= (bound w) x) (< x (bound w)) 
            (<= (bound height) y) (< y (bound height)))
        { (= i00 [inputImage (- i 4 w)])
          (= i01 [inputImage (- i w)])
          (= i02 [inputImage (- (+ i 4) w)])
          (= i10 [inputImage (- i 4)])
          (= i11 [inputImage i])
          (= i12 [inputImage (+ i 4)])
          (= i20 [inputImage (+ (- i 4) w)])
          (= i21 [inputImage (+ i w)])
          (= i22 [inputImage (+ i 4 w)])
          (= gx (+ (* kx00 i00) (* kx01 i01) (* kx02 i02)
                   (* kx10 i10) (* kx11 i11) (* kx12 i12)
                   (* kx20 i20) (* kx21 i21) (* kx22 i22)))
          (= gy (+ (* ky00 i00) (* ky01 i01) (* ky02 i02)
                   (* ky10 i10) (* ky11 i11) (* ky12 i12)
                   (* ky20 i20) (* ky21 i21) (* ky22 i22)))
          (= [outputImage i] ((int) (/ (sqrt (+ (* ((float) gx) gx) (* ((float) gy) gy))) 2))) })) 
  
  outputImage)

(grammar int (bound [int e])
  (+ (* (?? int) e) (?? int)))

; This is the completion of sobelFilter3 produced by the synthesizer using synth_3.
(procedure int* (sobelFilter3 [int* inputImage] [int width] [int height] [int pixelSize])
                  
  (: int* outputImage)
  (= outputImage ((int*) (malloc (* width height pixelSize))))
  (memset outputImage 0 (* width height pixelSize))
  
  (: int gx gy w i i00 i01 i02 i10 i11 i12 i20 i21 i22)
  (= w (* width 4))
  
  (for [(: int y in (range 0 (- height 1)))
        (: int x in (range 0 w))] 
    (= i (+ (* y w) x))
    (if (&& (<= (+ (* 0 ((int) w)) 4) x)
            (< x (+ (* 1 ((int) w)) -4))
            (<= (+ (* 0 ((int) height)) 1) y)
            (< y (+ (* 86 ((int) height)) 0)))  ; C0
        { (= i00 [inputImage (- i 4 w)])
          (= i01 [inputImage (- i w)])
          (= i02 [inputImage (- (+ i 4) w)])
          (= i10 [inputImage (- i 4)])
          (= i11 [inputImage i])
          (= i12 [inputImage (+ i 4)])
          (= i20 [inputImage (+ (- i 4) w)])
          (= i21 [inputImage (+ i w)])
          (= i22 [inputImage (+ i 4 w)])
          (= gx (+ (* kx00 i00) (* kx01 i01) (* kx02 i02)
                   (* kx10 i10) (* kx11 i11) (* kx12 i12)
                   (* kx20 i20) (* kx21 i21) (* kx22 i22)))
          (= gy (+ (* ky00 i00) (* ky01 i01) (* ky02 i02)
                   (* ky10 i10) (* ky11 i11) (* ky12 i12)
                   (* ky20 i20) (* ky21 i21) (* ky22 i22)))
          (= [outputImage i] ((int) (/ (sqrt (+ (* ((float) gx) gx) (* ((float) gy) gy))) 2))) }))
  
  outputImage)

; A refinement of sobelFilter3, in which we observe that the subexpression 
; (< y (+ (* 86 ((int) height)) 0) is always true, so it can be safely dropped.
; With this change, we see that the conditional C0 can be eliminated by pushing 
; the remaining bounds on x and y up to the loop head.  
(procedure int* (sobelFilter4 [int* inputImage] [int width] [int height] [int pixelSize])
                   
  (: int* outputImage)
  (= outputImage ((int*) (malloc (* width height pixelSize))))
  (memset outputImage 0 (* width height pixelSize))
  
  (: int gx gy w i i00 i01 i02 i10 i11 i12 i20 i21 i22)
  (= w (* width 4))
  
  (for [(: int y in (range 1 (- height 1)))
        (: int x in (range 4 (- w 4)))] 
    (= i (+ (* y w) x))
    (= i00 [inputImage (- i 4 w)])
    (= i01 [inputImage (- i w)])
    (= i02 [inputImage (- (+ i 4) w)])
    (= i10 [inputImage (- i 4)])
    (= i11 [inputImage i])
    (= i12 [inputImage (+ i 4)])
    (= i20 [inputImage (+ (- i 4) w)])
    (= i21 [inputImage (+ i w)])
    (= i22 [inputImage (+ i 4 w)])
    (= gx (+ (* kx00 i00) (* kx01 i01) (* kx02 i02)
             (* kx10 i10) (* kx11 i11) (* kx12 i12)
             (* kx20 i20) (* kx21 i21) (* kx22 i22)))
    (= gy (+ (* ky00 i00) (* ky01 i01) (* ky02 i02)
             (* ky10 i10) (* ky11 i11) (* ky12 i12)
             (* ky20 i20) (* ky21 i21) (* ky22 i22)))
    (= [outputImage i] ((int) (/ (sqrt (+ (* ((float) gx) gx) (* ((float) gy) gy))) 2))))
  
  outputImage)

; A refinement of sobelFilter4 in which we substitute constants for 
; kxij and kyij and simplify the resulting expressions.
(procedure int* (sobelFilter5 [int* inputImage] [int width] [int height] [int pixelSize])
                 
  (: int* outputImage)
  (= outputImage ((int*) (malloc (* width height pixelSize))))
  (memset outputImage 0 (* width height pixelSize))
  
  (: int gx gy w i i00 i01 i02 i10 i11 i12 i20 i21 i22)
  (= w (* width 4))
  
  (for [(: int y in (range 1 (- height 1)))
        (: int x in (range 4 (- w 4)))] 
    (= i (+ (* y w) x))
    (= i00 [inputImage (- i 4 w)])
    (= i01 [inputImage (- i w)])
    (= i02 [inputImage (- (+ i 4) w)])
    (= i10 [inputImage (- i 4)])
    (= i11 [inputImage i])
    (= i12 [inputImage (+ i 4)])
    (= i20 [inputImage (+ (- i 4) w)])
    (= i21 [inputImage (+ i w)])
    (= i22 [inputImage (+ i 4 w)])
    (= gx (+ i00 (* 2 i01) i02 (* -1 i20) (* -2 i21) (* -1 i22)))
    (= gy (+ i00 (* -1 i02) (* 2 i10) (* -2 i12) i20 (* -1 i22)))
    (= [outputImage i] ((int) (/ (sqrt (+ (* ((float) gx) gx) (* ((float) gy) gy))) 2))))
  
  outputImage)

;;; ------------ Verification of refinement steps ------------ ;;;

; Given two arrays of the same size, checks that they hold the same 
; values at each index. 
(procedure void (check [int* actual] [int* expected] [int SIZE])
  (assert (>= SIZE 0))
  (for [(: int i in (range SIZE))]
    (assert (== [actual i] [expected i]))))

(: int pixelSize)
(= pixelSize (* 4 (sizeof int)))

; Verify that sobelFilter0 and sobelFilter1 are equivalent (~30 sec).
(procedure void (verify_0_1)
   (verify #:forall [(: int width in (range 1 10))
                     (: int height in (range 1 10))
                     (: int[(* width height pixelSize)] inputImageData)]
           #:ensure (check (sobelFilter0 inputImageData width height pixelSize)
                           (sobelFilter1 inputImageData width height pixelSize)
                           (* width height pixelSize))))

; Verify that sobelFilter1 and sobelFilter2 are equivalent (~30 sec).
(procedure void (verify_1_2)
   (verify #:forall [(: int width in (range 1 10))
                     (: int height in (range 1 10))
                     (: int[(* width height pixelSize)] inputImageData)]
           #:ensure (check (sobelFilter1 inputImageData width height pixelSize)
                           (sobelFilter2 inputImageData width height pixelSize)
                           (* width height pixelSize))))

; Synthesizes sobelFilter3 using sobelFilter2 as a reference (~20 sec).
(procedure void (synth_3)
  (synth #:forall [(: int width in (range 1 5))
                   (: int height in (range 1 5))
                   (: int[(* width height pixelSize)] inputImageData)]
         #:bitwidth 8
         #:ensure (check (sobelFilter2 inputImageData width height pixelSize)
                         (sobelFilter3Sketch inputImageData width height pixelSize)
                         (* width height pixelSize))))

; Verify that the synthesized refinement is equivalent to sobelFilter2 larger scopes (~27 sec).
(procedure void (verify_2_3)
  (verify #:forall [(: int width in (range 1 10))
                    (: int height in (range 1 10))
                    (: int[(* width height pixelSize)] inputImageData)]
          #:ensure (check (sobelFilter2 inputImageData width height pixelSize)
                          (sobelFilter3 inputImageData width height pixelSize)
                          (* width height pixelSize))))     

; Verify that sobelFilter3 and sobelFilter4 are equivalent (~25 sec).
(procedure void (verify_3_4)
  (verify #:forall [(: int width in (range 1 10))
                    (: int height in (range 1 10))
                    (: int[(* width height pixelSize)] inputImageData)]
          #:ensure (check (sobelFilter3 inputImageData width height pixelSize)
                          (sobelFilter4 inputImageData width height pixelSize)
                          (* width height pixelSize)))) 
          
; Verify that sobelFilter4 and sobelFilter5 are equivalent (~25 sec).
(procedure void (verify_4_5)
  (verify #:forall [(: int width in (range 1 10))
                    (: int height in (range 1 10))
                    (: int[(* width height pixelSize)] inputImageData)]
          #:ensure (check (sobelFilter4 inputImageData width height pixelSize)
                          (sobelFilter5 inputImageData width height pixelSize)
                          (* width height pixelSize)))) 

; Everything can be made faster if the built-in (precise) square root procedure 
; is replaced with a no-op.  This is actually ok to do because our correctness 
; condition doesn't depend on any properties of square roots.
; (procedure float (sqrt [float val]) val)
