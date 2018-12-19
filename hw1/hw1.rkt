#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

(test)

; Problem one

; takes nautical miles, returns kilometers
(: nm->km : Exact-Rational -> Exact-Rational)
(define (nm->km nm)
  (/ (* nm 1852) 1000))
(check-expect (nm->km 1000) 1852)

; takes kilometers, returns nautical miles
(: km->nm : Exact-Rational -> Exact-Rational)
(define (km->nm km)
  (/ (* km 1000) 1852))
(check-expect (km->nm 1852) 1000)

; takes coins (number of quarters, dimes, nickels, pennies)
; and returns total value in cents
(: coins->cents : Integer Integer Integer Integer -> Integer)
(define (coins->cents q d n p)
  (+ (* q 25) (* d 10) (* n 5) p))
(check-expect (coins->cents 1 1 1 1) 41)

; takes in a, b, c, x and evaluates polynomial expression:
; ax^2 + bx + c
(: eval-quad : Real Real Real Real -> Real)
(define (eval-quad a b c x)
  (+ (* (sqr x) a) (* b x) c))
(check-within (eval-quad 4 3 2 1) 9 0.001)

; takes time fallen (secs) and returns distance (m)
(: distance-fallen : Real -> Real)
(define (distance-fallen t)
  (* (sqr t) 4.9))
(check-within (distance-fallen 2) 19.6 0.001)

; takes distance fallen (m) and returns time (secs)
(: seconds-to-fall : Real -> Real)
(define (seconds-to-fall d)
  (sqrt (* d .204)))
(check-within (seconds-to-fall 1) .452 0.001)

; Problem 2
; returns true if given value is > .5, but < 1.0, otherwise false
(: majority? : Real -> Boolean)
(define (majority? n)
  (> n .5))
(check-expect (majority? .5) #f)

; returns true if water would be liquid @ given temperature
(: liquid-water? : Real -> Boolean)
(define (liquid-water? t)
  (> t 32))
(check-expect (liquid-water? 32) #f)

; returns true if given point lies above given parabola
(: above-quad? : Real Real Real Real Real -> Boolean)
(define (above-quad? a b c x y)
  (> y (+ (* (sqr x) a) (* b x) c)))
(check-expect (above-quad? 4 3 2 1 10) #t)

; Problem 3
; returns x-val of midpoint of two points
(: midpt-x : Real Real Real Real -> Real)
(define (midpt-x x1 y1 x2 y2)
  (/ (+ x1 x2) 2))
(check-within (midpt-x 0 0 4 4) 2 0.001)

; returns y-val of midpoint of two points
(: midpt-y : Real Real Real Real -> Real)
(define (midpt-y x1 y1 x2 y2)
  (/ (+ y1 y2) 2))
(check-within (midpt-y 0 0 4 4) 2 0.001)

; Problem 4
; returns a / b rounded up
(: div-ceiling : Integer Integer -> Integer)
(define (div-ceiling a b)
  (exact-ceiling (/ a b)))
(check-expect (div-ceiling 10 3) 4)

; returns num cars needed for given num bikes and people
(: cars-needed : Integer Integer -> Integer)
(define (cars-needed p b)
  (max (div-ceiling p 4) (div-ceiling b 2)))
(check-expect (cars-needed 8 4) 2)

; returns even num cars
(: cars-pairs : Integer Integer -> Integer)
(define (cars-pairs p b)
  (* (div-ceiling (cars-needed p b) 2) 2))
(check-expect (cars-pairs 10 4) 4)

(test)