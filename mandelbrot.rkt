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

(define (make-palette nb-split colors)
  (define (gradient nb-steps s e)
    (let* ([spread (- e s)]
           [step (/ spread nb-steps)])
      (for/list ([i (in-range nb-steps)])
        (round (+ s (* i step))))))
  (define (rgb-gradient nb-steps s e)
    (for/list ([r (in-list (gradient nb-steps (car s) (car e)))]
               [g (in-list (gradient nb-steps (cadr s) (cadr e)))]
               [b (in-list (gradient nb-steps (caddr s) (caddr e)))])
      (list r g b)))
  (define (palette-helper nb-steps colors)
    (if (eq? (cdr colors) '())
        (list (car colors))
        (append (rgb-gradient nb-steps (car colors)(cadr colors))(palette-helper nb-steps (cdr colors)))))
  (list->vector (map (lambda (c)(make-object pen% (make-object color% (car c) (cadr c) (caddr c)) 1 'solid)) (palette-helper (+ nb-split 1) colors))))

;(define palette (make-palette 5 '((127 0 0)(255 127 0)(127 0 0))))
(define palette (make-palette 5 '((0 0 64)(0 255 255)(255 128 0)(0 0 64))))
(define palette-size (vector-length palette))


(define (draw-mandelbrot dc)
  (let-values (((w h) (send dc get-size)))
    (send dc set-pen no-pen)
    (send dc set-brush black-brush)
    (send dc draw-rectangle 0 0 w h)
    (send dc set-brush no-brush)
    (send dc set-pen white-pen)
    (define-values (conv-x conv-y) (get-converters w h -2.5 1 -1.5 1.5))
    (define cxs (list->vector (for/list ([x (in-range w)]) (conv-x x))))
    (define cys (list->vector (for/list ([y (in-range h)]) (conv-y y))))
    (for* ([y (in-range h)]
           [x (in-range w)])
      (let ([rank (escape (vector-ref cxs x) (vector-ref cys y) 30)])
        (cond [rank (send dc set-pen (vector-ref palette (modulo rank palette-size)))
                    (send dc draw-point x y)])))))

; Show the frame
(send frame show #t)
