#lang typed/racket

(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

; Problem 1
; returns smallest possible plane given number of passengers
(: assign-plane : Integer -> String)
(define (assign-plane n)
  (cond
    [(< n 0) (error "assign-plane: number of passengers should not be negative")]
    [(= n 0) "Recommendation: cancel flight"]
    [(<= n 128) "Recommendation: 319"]
    [(<= n 150) "Recommendation: 320"]
    [(<= n 166) "Recommendation: 737-800"]
    [(<= n 179) "Recommendation: 737-900"]
    [(<= n 213) "Recommendation: 757"]
    [(<= n 242) "Recommendation: 767"]
    [(<= n 366) "Recommendation: 777"]
    [else "Recommendation: split flights"]))
(check-expect (assign-plane 340) "Recommendation: 777")
(check-expect (assign-plane 400) "Recommendation: split flights")
(check-error (assign-plane -20) "assign-plane: number of passengers should not be negative")

; Problem 2
; records number of flight segments, number of miles flown
(define-struct FlightHistory
  ([segments : Integer]
   [miles    : Integer]))

; returns number of miles earned, given FlightHistory
(: miles-earned : FlightHistory -> Integer)
(define (miles-earned f)
  (match f
    [(FlightHistory s m)
     (cond
       [(or (>= s 50) (>= m 70000)) (* m 2)]
       [(or (>= s 25) (>= m 35000)) (exact-ceiling (* m 1.25))]
       [(or (< s 0) (< m 0)) (error "miles-earned: segments/miles cannot be negative")]
       [else m])]))
(check-expect (miles-earned (FlightHistory 20 20000)) 20000)
(check-expect (miles-earned (FlightHistory 30 20000)) 25000)
(check-expect (miles-earned (FlightHistory 50 20000)) 40000)
(check-error (miles-earned (FlightHistory -50 -200)) "miles-earned: segments/miles cannot be negative")

; Problem 3
; tracks counts for each seat-class
(define-struct Seats
  ([first    : Integer]
   [business : Integer]
   [coach    : Integer]))

; return total number of passengers on flight
(: passengers : Seats -> Integer)
(define (passengers s)
  (match s
    [(Seats f b c)
     (cond
       [(or (< f 0) (< b 0) (< c 0)) (error "passengers: number of passengers cannot be negative")]
       [else (+ f b c)])]))
(check-expect (passengers (Seats 50 50 50)) 150)
(check-error (passengers (Seats -5 10 10)) "passengers: number of passengers cannot be negative")

; combine two Seats structs into one
(: combine-flights : Seats Seats -> Seats)
(define (combine-flights a b)
  (match (list a b)
    [(list (Seats fa ba ca) (Seats fb bb cb))
     (cond
       [(or (< fa 0) (< ba 0) (< ca 0) (< fb 0) (< bb 0) (< cb 0)) (error "combine-flights: number of passengers cannot be negative")]
       [else (Seats (+ fa fb) (+ ba bb) (+ ca cb))])]))
(check-expect (combine-flights (Seats 50 50 50) (Seats 50 50 50)) (Seats 100 100 100))
(check-error (combine-flights (Seats 50 50 50) (Seats 50 50 -50)) "combine-flights: number of passengers cannot be negative")

; return whether the given aircraft can seat all passengers in their desired class
(: can-fit? : Seats Seats -> Boolean)
(define (can-fit? a b)
  (match (list a b)
    [(list (Seats fa ba ca) (Seats fb bb cb))
     (cond
       [(or (< fb 0) (< bb 0) (< cb 0)) (error "can-fit?: number of passengers cannot be negative")]
       [(or (< fa 0) (< ba 0) (< ca 0)) (error "can-fit?: capacity can not be negative")]
       [else (and (>= fa fb) (>= ba bb) (>= ca cb))])]))
(check-expect (can-fit? (Seats 51 51 51) (Seats 50 50 50)) #t)
(check-expect (can-fit? (Seats 50 51 51) (Seats 51 50 50)) #f)
(check-error (can-fit? (Seats 50 50 50) (Seats 50 50 -50)) "can-fit?: number of passengers cannot be negative")

; return whether all the premium seats have been sold given aircraft, ticket sales
(: max-premium? : Seats Seats -> Boolean)
(define (max-premium? a b)
  (match (list a b)
    [(list (Seats fa ba _) (Seats fb bb _)) (and (= fa fb) (= ba bb))]))
(check-expect (max-premium? (Seats 50 50 50) (Seats 50 50 50)) #t)
(check-expect (max-premium? (Seats 50 51 50) (Seats 48 51 50)) #f)

; return new passenger locations with upgraded seats
(: upgrade : Seats Seats -> Seats)
(define (upgrade a b)
  (match (list a b)
    [(list (Seats fa ba ca) (Seats fb bb cb))
     (cond
       [(max-premium? a b) b]
       [(or (< fa 0) (< ba 0) (< ca 0) (< fb 0) (< bb 0) (< cb 0)) (error "upgrade: cannot have negative capacity/passengers")]
       [(< (passengers a) (passengers b)) (error "upgrade: plane over capacity")]
       [(and (> fa fb) (> bb 0)) (upgrade a (Seats (+ 1 fb) (- bb 1) cb))]
       [(and (> fa fb) (= bb 0) (> cb 0)) (upgrade a (Seats (+ 1 fb) bb (- cb 1)))]
       [(and (= fa fb) (> ba bb) (> cb 0)) (upgrade a (Seats fb (+ 1 bb) (- cb 1)))]
       [else (error "upgrade: something weird happened")])]))
(check-expect (upgrade (Seats 50 50 50) (Seats 30 40 70)) (Seats 50 50 40))
(check-error (upgrade (Seats 50 50 -50) (Seats 30 40 70)) "upgrade: cannot have negative capacity/passengers")
(check-error (upgrade (Seats 10 10 10) (Seats 30 40 70)) "upgrade: plane over capacity")

; generate string of people represented by plane section
(: seats->letters : Seats -> String)
(define (seats->letters s)
  (match s
   [(Seats f b c)
    (cond
     [(or (< f 0) (< b 0) (< c 0)) (error "seats->letters: cannot have negative seats")]
     [else (string-append (make-string f #\F) (make-string b #\B) (make-string c #\C))])]))
(check-expect (seats->letters (Seats 2 3 5)) "FFBBBCCCCC")
(check-error (seats->letters (Seats 2 -3 5)) "seats->letters: cannot have negative seats")

(test)