#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")


(: dt Positive-Real)
(define dt 1/30)

(define-struct Ball
  ([y-position : Real] ;; the height of the ball's center in meters, with the ground y=0
   [radius     : Integer] ;; in meters
   [velocity   : Real] ;; in meters per second
   [elasticity : Real] ;; a number between 0 and 1
   [color      : Image-Color]))

(define-struct World
  ([ball1 : Ball]
   [ball2 : Ball]
   [ball3 : Ball]
   [background : Image]
   [time-elapsed : Real]))

;; return new velocity given position, radius, current velocity,
;; elasticity, time elapsed (manages ball bouncing)
(: tick-velocity (Real Real Real Real Real -> Real))
(define (tick-velocity y r v e t)
  (cond
    [(and (>= y (- window r)) (>= v 0)) (* v e -1)]
    [else (+ v (* t 9.8))]))
(check-within (tick-velocity window 10 10 1 .1) -10 0.001)

;; return new position given Ball, dt
(: tick-pos (Ball Real -> Real))
(define (tick-pos b t)
  (match b
   [(Ball y r v e c) (check-y (+ y (* (tick-velocity y r v e t) t)) r)]))

;; make sure ball doesn't sink out of the window
(: check-y : Real Real -> Real)
(define (check-y y r)
  (cond
    [(> y (- window r)) (- window r)]
    [else y]))

;; update ball with new velocity, postion
(: tick-ball (Ball Real -> Ball))
(define (tick-ball b t)
  (match b
    [(Ball y r v e c) (Ball (tick-pos b t) r (tick-velocity y r v e t) e c)]))
(check-within (tick-ball (Ball 100 10 0 1 "blue") 1) (Ball 109.8 10 9.8 1 "blue") .001)

;; draw ball
(: draw-ball (Ball Real -> Image))
(define (draw-ball b x)
  (match b
    [(Ball y r v e c) (place-image (circle r "solid" c) x y bgt)]))

;; draw world w 3 balls
(: draw-world (World -> Image))
(define (draw-world w)
  (match w
    [(World bl1 bl2 bl3 bg _) (overlay (draw-ball bl1 (/ window 6)) (draw-ball bl2 (/ window 2)) (draw-ball bl3 (* window 5/6)) bg)])) 

;; update world with new balls
(: react-to-tick (World -> World))
(define (react-to-tick w)
  (match w
  [(World b1 b2 b3 bg t) (World (tick-ball b1 t) (tick-ball b2 t) (tick-ball b3 t) bg t)]))

;; open, run window
(: run-simulation (Ball Ball Ball Image -> World))
(define (run-simulation b1 b2 b3 bg)
  (big-bang (World b1 b2 b3 bg dt) : World
    [to-draw draw-world]
    [on-tick react-to-tick dt]))

(: window : Real)
(define window 600)

(define b1
  (Ball 2 2 0 .2 "darkblue"))

(define b2
  (Ball 20 20 0 .6 "lightgray"))

(define b3
  (Ball 10 10 0 .8 "lavender"))

(define bgs
  (square window "solid" "lightseagreen"))

(define bgt
   (square window 0 "white"))

(test)