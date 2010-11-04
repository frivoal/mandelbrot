#!/usr/bin/gracket
#lang racket

; Copyright 2010 Florian Rivoal <frivoal@gmail.com>
; 
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

(define black-brush (make-object brush% "BLACK" 'solid))
(define black-pen (make-object pen% "BLACK" 1 'solid))
(define no-pen (make-object pen% "BLACK" 1 'transparent))

(define (make-palette nb-split colors)
  (define (gradient nb-steps s e)
    (let ([step (/ (- e s) nb-steps)])
      (build-list nb-steps
        (lambda (i) (round (+ s (* i step)))))))
  (define (rgb-gradient nb-steps s e)
    (map (lambda (r g b) (list r g b))
         (gradient nb-steps (car s) (car e))
         (gradient nb-steps (cadr s) (cadr e))
         (gradient nb-steps (caddr s) (caddr e))))
  (define (palette-helper nb-steps colors)
    (if (eq? (cdr colors) '())
        (list (car colors))
        (append (rgb-gradient nb-steps (car colors)(cadr colors))
                (palette-helper nb-steps (cdr colors)))))
  (let ([colors (map (lambda (c) (make-object color% (car c) (cadr c) (caddr c)))
                     (palette-helper (+ nb-split 1) colors))])
    (values
      (list->vector (map (lambda (c) (make-object pen% c 1 'solid)) colors))
      (list->vector (map (lambda (c) (make-object brush% c 'solid)) colors)))))

;(define-values (pens brushes) (make-palette 15 '((127 0 0)(255 127 0)(127 0 0))))
(define-values (pens brushes) (make-palette 15 '((0 0 64)(0 255 255)(255 128 0)(0 0 64))))
(define palette-size (vector-length pens))

(define painter-thread #f)
(define top 1.5)
(define bottom -1.5)
(define left -2.5)
(define right 1)
(define iter 30)
(define cached #f)

(define (draw-mandelbrot dc)
  (cond [(thread? painter-thread)
    (kill-thread painter-thread)])
  (set! painter-thread (thread (lambda ()
    (let-values ([(w h) (send dc get-size)])
      (send dc set-pen no-pen)
      (define current (list w h left right bottom top))
      (cond [(not (equal? cached current))
             (for ([i (in-list '(16 4))])
               (define zw (ceiling (/ w i)))
               (define zh (ceiling (/ h i)))
               (define-values (conv-x conv-y) (get-converters zw zh left right bottom top))
               (define cxs (list->vector (for/list ([x (in-range zw)]) (conv-x x))))
               (define cys (list->vector (for/list ([y (in-range zh)]) (conv-y y))))
               (for* ([y (in-range 0 zh)]
                      [x (in-range 0 zw)])
                 (let ([rank (escape (vector-ref cxs x) (vector-ref cys y) iter)])
                   (if rank
                       (send dc set-brush (vector-ref brushes (modulo rank palette-size)))
                       (send dc set-brush black-brush))
                   (send dc draw-rectangle (* x i) (* y i) i i))))])
      (set! cached current)
      (define-values (conv-x conv-y) (get-converters w h left right bottom top))
      (define cxs (list->vector (for/list ([x (in-range w)]) (conv-x x))))
      (define cys (list->vector (for/list ([y (in-range h)]) (conv-y y))))
      (for* ([y (in-range h)]
             [x (in-range w)])
        (let ([rank (escape (vector-ref cxs x) (vector-ref cys y) iter)])
          (if rank
            (send dc set-pen (vector-ref pens (modulo rank palette-size)))
            (send dc set-pen black-pen))
          (send dc draw-point x y))))))))

(define frame (new frame% [label "Mandelbrot"]
                          [width 300]
                          [height 300]))

(define mandelbrot-canvas%
  (class canvas%
    (define/override (on-event event)
      (cond [(send event button-down? 'left)
             (local [(define x (send event get-x))
                     (define y (send event get-y))
                     (define-values (w h) (send this get-size))
                     (define-values (conv-x conv-y) (get-converters w h left right bottom top))
                     (define cx (+ left (* (- right left) (/ x w))))
                     (define cy (+ bottom (* (- top bottom) (/ (- h y) h))))
                     (define zw (- (conv-x w) (conv-x 0)))
                     (define zh (- (conv-y 0) (conv-y h)))]
               (set! left (- cx (/ zw 2.5)))
               (set! right (+ cx (/ zw 2.5)))
               (set! bottom (- cy (/ zh 2.5)))
               (set! top (+ cy (/ zh 2.5)))
               (send this on-paint))]
            [(send event button-down? 'right)
             (set! iter (+ 10 iter))
             (send this on-paint)]))
    (super-new)))

(define canvas
  (new mandelbrot-canvas% [parent frame]
               [style (list 'no-autoclear)]
               [paint-callback
                (lambda (canvas dc) (draw-mandelbrot dc))]))

(send frame show #t)
