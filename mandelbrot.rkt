#lang racket

(require racket/gui/base)

(define (escape cx cy limit)
  (define (escape-helper cx cy acc-x acc-y n)
    (let ([acc-x2 (sqr acc-x)]
          [acc-y2 (sqr acc-y)])
      (cond
        [(> (+ acc-x2 acc-y2) 4) n]
        [(= n limit) #f]
        [else (escape-helper cx cy (+ acc-x2 (- acc-y2) cx) (+ (* 2 acc-x acc-y ) cy) (+ 1 n) )])))
  (escape-helper cx cy cx cy 0))

(define (get-converters w h x1 x2 y1 y2)
  (define-values (cx1 cx2 cy1 cy2)
    (if (< (/ w h) (/ (- x2 x1) (- y2 y1)))
      (let ([y-center (/ (+ y2 y1) 2)]
            [y-range (/ (* (/ (- x2 x1) w) h) 2)])
            (values x1 x2 (- y-center y-range) (+ y-center y-range)))
      (let ([x-center (/ (+ x2 x1) 2)]
            [x-range (/ (* (/ (- y2 y1) h) w) 2)])
            (values (- x-center x-range) (+ x-center x-range) y1 y2))))
  (values
    (lambda (x) (+ cx1 (* (- cx2 cx1) (/ (+ x 0.5) w))))
    (lambda (y) (+ cy1 (* (- cy2 cy1) (/ (- h (+ y 0.5)) h))))))


; Make a 300 x 300 frame
  (define frame (new frame% [label "Mandelbrot"]
                            [width 300]
                            [height 300]))
; Make the drawing area with a paint callback
(define canvas
  (new canvas% [parent frame]
               [paint-callback
                (lambda (canvas dc) (draw-mandelbrot dc))]))

; Get the canvas's drawing context
(define dc (send canvas get-dc))

; Make some pens and brushes
(define no-pen (make-object pen% "BLACK" 1 'transparent))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define black-brush (make-object brush% "BLACK" 'solid))
(define white-pen (make-object pen% "WHITE" 1 'solid))

; Make the color palete
(define palete-size 32)
(define palete 
  (let ([part1 (for/list ([i (in-range 0 (/ palete-size 3))])
    (let ([step (floor (/ 256 (/ palete-size 3)))])
      (make-object pen%
        (make-object color%
          0
          (* i step )
          (+ 64 (floor (* i (/ (* 3 step) 4)))))
        1 'solid)))]
       [part2 (for/list ([i (in-range 0 (/ palete-size 3))])
    (let ([step (floor (/ 256 (/ palete-size 3)))])
      (make-object pen%
        (make-object color%
          (* i step)
          (- 255 (floor (* i (/ (* 3 step) 4))))
          (- 255 (* i step)))
        1 'solid)))]
       [part3 (for/list ([i (in-range 0 (/ palete-size 3))])
    (let ([step (floor (/ 256 (/ palete-size 3)))])
      (make-object pen%
        (make-object color%
          (- 255 (* i step))
          (- 127 (floor (* i (/ step 2))))
          (floor (* i (/ step 4))))
        1 'solid)))])
    (list->vector (append part1 part2 part3))))

;(define palete 
;  (let ([half (for/list ([i (in-range 0 (/ palete-size 2))])
;    (let ([step (floor (/ 256 (/ palete-size 2)))])
;      (make-object pen%
;        (make-object color%
;          0
;          (* i step )
;          (+ 64 (* i (/ (* 3 step) 4))))
;        1 'solid)))])
;    (list->vector (append half (reverse half)))))

;(define palete 
;  (let ([half (for/list ([i (in-range 0 (/ palete-size 2))])
;    (let ([step (floor (/ 256 (/ palete-size 2)))])
;      (make-object pen%
;        (make-object color%
;          (+ 127 (* i (/ step 2)))
;          (* i (/ step 2))
;          0)
;        1 'solid)))])
;    (list->vector (append half (reverse half)))))


(define (draw-mandelbrot dc)
  (let-values (((w h) (send dc get-size)))
    (send dc set-pen no-pen)
    (send dc set-brush black-brush)
    (send dc draw-rectangle 0 0 w h)
    (send dc set-brush no-brush)
    (send dc set-pen white-pen)
    (define-values (conv-x conv-y) (get-converters w h -2.5 1 -1.5 1.5))
    (for ([y (in-range 0 h)])
      (for ([x (in-range 0 w)])
        (let ([rank (escape (conv-x x) (conv-y y) 50)])
          (cond [rank (send dc set-pen (vector-ref palete (modulo rank palete-size)))(send dc draw-point x y)] ))))))

; Show the frame
(send frame show #t)
