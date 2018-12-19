#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

(define-struct Finish
  ([lname : String]
   [fname : String]
   [time-millis : Integer]
   [school : String]
   [grade : Integer]))

(define-struct (Ranked a)
  ([rank : Integer]
   [item : a]))

;; Display the time as minutes, seconds, and milliseconds, in the form "x:xx.xxx".
; ms to minutes
(: ms-min (Integer -> String))
(define (ms-min ms)
  (number->string (quotient ms 60000)))
(check-expect (ms-min 60000) "1")

;ms to seconds
(: ms-sec (Integer -> String))
(define (ms-sec ms)
  (0append (number->string (quotient (modulo ms 60000) 1000))))
(check-expect (ms-sec 30500) "30")

; ms to ms
(: ms-ms (Integer -> String))
(define (ms-ms ms)
  (local {(define t (number->string ms))}
   (string-append "."(substring t (- (string-length t) 3) (string-length t)))))
(check-expect (ms-ms 539090) ".090")
  
; append zeroes to s
(: 0append (String -> String))
(define (0append s)
  (cond
    [(= (string-length s) 2) s]
    [(< (string-length s) 2) (0append(string-append "0" s))]
    [else (error "0append: string too long")]))

; display time
(: format-time (Integer -> String))
(define (format-time t)
  (string-append (ms-min t) ":" (0append (ms-sec t)) (ms-ms t)))
(check-expect (format-time 61234) "1:01.234")

;; Check that the list of finishing times is strictly ascending.

; compare two given Finishes
(: finish< : Finish Finish -> Boolean)
(define (finish< f1 f2)
  (match (list f1 f2)
    [(list (Finish _ _ t1 _ _) (Finish _ _ t2 _ _))
           (< t1 t2)]))

(: strictly-ascending-times? ((Listof Finish) -> Boolean))
(define (strictly-ascending-times? fs)
  (match fs
    ['() #t]
    [(cons head '()) #t]
    [(cons head tail) (and (finish< head (list-ref fs 1)) (strictly-ascending-times? tail))]))
(check-expect (strictly-ascending-times? sample-data) #t)

; find finish for named person
(: find-finish (String String (Listof Finish) -> (U Finish 'NotFound)))
(define (find-finish ln fn fs)
  (match (list ln fn fs)
    [(list _ _ '()) 'NotFound]
    [(list ln fn (cons (Finish l f t s g) tail))
      (cond
        [(and (string=? ln l) (string=? fn f)) (Finish l f t s g)]
        [else (find-finish ln fn tail)])]))
(check-expect (find-finish "Green" "Peggy" sample-data) (Finish "Green" "Peggy" 553486 "Central" 5))
(check-expect (find-finish "sdfasdf" "sdfoj" sample-data) 'NotFound)

;; Return all the finishes from the specified grade.
(: from-grade (Integer (Listof Finish) -> (Listof Finish)))
(define (from-grade g fs)
  (match (list g fs)
    [(list _ '()) '()]
    [(list g (cons (Finish l f t s gr) tail))
      (cond
        [(= g gr) (cons (Finish l f t s gr) (from-grade g tail))]
        [else (from-grade g tail)])]))
(check-expect (from-grade 8 sample-data)
              (list
 (Finish "Kumar" "Jennifer" 552342 "Central" 8)
 (Finish "Williams" "Beyonce" 555783 "Harper" 8)
 (Finish "McBride" "Paula" 557313 "Mercy" 8)
 (Finish "Strickland" "Cecilia" 560445 "Pepper" 8)
 (Finish "Green" "Pauline" 563466 "Central" 8)
 (Finish "Miller" "Mia" 564866 "Harper" 8)
 (Finish "Anderson" "Kathy" 568258 "Southeast" 8)
 (Finish "Miller" "Rebecca" 577740 "Harper" 8)
 (Finish "Mandelbrot" "Mia" 582624 "Southeast" 8)
 (Finish "Bingham" "Kathy" 583331 "Harper" 8)
 (Finish "Cameron" "Kathy" 586389 "Ridgemont" 8)
 (Finish "Cohen" "Jennifer" 586732 "Harper" 8)
 (Finish "Johnson" "Zelda" 587009 "West" 8)
 (Finish "Douglas" "Pavithra" 595182 "Harper" 8)
 (Finish "Ramsey" "Mia" 595465 "Ridgemont" 8)
 (Finish "Johnson" "Sophia" 603051 "Laurel" 8)
 (Finish "Black" "Sally" 603401 "West" 8)
 (Finish "Kumar" "Sophia" 607778 "Ridgemont" 8)
 (Finish "Bingham" "Rachel" 609177 "Mercy" 8)))

;; Return all the finishes from the specified school.
(: from-school (String (Listof Finish) -> (Listof Finish)))
(define (from-school sc fs)
  (match (list sc fs)
    [(list _ '()) '()]
    [(list sc (cons (Finish l f t s g) tail))
      (cond
        [(string=? sc s) (cons (Finish l f t s g) (from-school sc tail))]
        [else (from-school sc tail)])]))
(check-expect (from-school "Central" sample-data)
              (list
 (Finish "Kumar" "Jennifer" 552342 "Central" 8)
 (Finish "Green" "Peggy" 553486 "Central" 5)
 (Finish "Rosen" "Pavithra" 557916 "Central" 7)
 (Finish "McBride" "Margaret" 559266 "Central" 5)
 (Finish "Cohen" "Melissa" 561588 "Central" 5)
 (Finish "Green" "Pauline" 563466 "Central" 8)
 (Finish "Cameron" "Margaret" 570560 "Central" 7)
 (Finish "Mandelbrot" "Alexandra" 573950 "Central" 7)
 (Finish "Miller" "Martha" 581324 "Central" 6)
 (Finish "Brown" "Elizabeth" 584290 "Central" 7)
 (Finish "Patel" "Anne" 584533 "Central" 6)
 (Finish "Jones" "Samantha" 591396 "Central" 7)
 (Finish "Williams" "Alice" 602112 "Central" 7)))

; return top n finishes
(: top-n (Integer (Listof Finish) -> (Listof Finish)))
(define (top-n n fs)
  (match (list n fs)
    [(list _ '()) '()]
    [(list n (cons head tail))
     (cond
       [(> n 0) (cons head (top-n (- n 1) tail))]
       [else '()])]))
(check-expect (top-n 5 sample-data)  (list
   (Finish "Petersen" "Rhea" 550588 "Pepper" 5)
   (Finish "Kumar" "Jennifer" 552342 "Central" 8)
   (Finish "Jones" "Zelda" 552521 "Pepper" 5)
   (Finish "Rosen" "Marci" 552961 "Pepper" 7)
   (Finish "Anderson" "Pauline" 553188 "West" 7)))

; Return the top n finishes from the named school.
(: top-n-from-school (Integer String (Listof Finish) -> (Listof Finish)))
(define (top-n-from-school n sc fs)
  (top-n n (from-school sc fs)))
(check-expect (top-n-from-school 5 "Central" sample-data)
              (list
 (Finish "Kumar" "Jennifer" 552342 "Central" 8)
 (Finish "Green" "Peggy" 553486 "Central" 5)
 (Finish "Rosen" "Pavithra" 557916 "Central" 7)
 (Finish "McBride" "Margaret" 559266 "Central" 5)
 (Finish "Cohen" "Melissa" 561588 "Central" 5)))

; return top n finishes from grade
(: top-n-from-grade (Integer Integer (Listof Finish) -> (Listof Finish)))
(define (top-n-from-grade n gr fs)
  (top-n n (from-grade gr fs)))
(check-expect (top-n-from-grade 5 8 sample-data) (list
 (Finish "Kumar" "Jennifer" 552342 "Central" 8)
 (Finish "Williams" "Beyonce" 555783 "Harper" 8)
 (Finish "McBride" "Paula" 557313 "Mercy" 8)
 (Finish "Strickland" "Cecilia" 560445 "Pepper" 8)
 (Finish "Green" "Pauline" 563466 "Central" 8)))

;; Compute the mean finishing time.
(: sumf : (Listof Finish) -> Integer)
(define (sumf fs)
  (match fs
    ['() 0]
    [(cons (Finish _ _ t _ _) tail) (+ t (sumf tail))]))

(: mean-time ((Listof Finish) -> Integer))
(define (mean-time fs)
  (quotient (sumf fs) (length fs)))
(check-expect (mean-time sample-data) 581952)

;; Assign ranks to every finish in the list, starting at 1.
(: rankf : (Listof Finish) Integer -> (Listof (Ranked Finish)))
(define (rankf fs l)
  (match (list fs l)
    [(list '() _) '()]
    [(list (cons head tail) l) (cons (Ranked (+ 1 (- l (length fs))) head) (rankf tail l))]))

(: rank-race : (Listof Finish) -> (Listof (Ranked Finish)))
(define (rank-race fs)
  (rankf fs (length fs)))
;; not doing by hand, checked easily in command line

;; sum of the ranks of the top three finishes.
(: top-nr : Integer (Listof (Ranked Finish)) -> (Listof (Ranked Finish)))
(define (top-nr n fs)
  (match (list n fs)
    [(list _ '()) '()]
    [(list n (cons head tail))
     (cond
       [(> n 0) (cons head (top-nr (- n 1) tail))]
       [else '()])]))

(: from-schoolr : String (Listof (Ranked Finish)) -> (Listof (Ranked Finish)))
(define (from-schoolr sc fs)
  (match (list sc fs)
    [(list _ '()) '()]
    [(list sc (cons (Ranked r (Finish l f t s g)) tail))
      (cond
        [(string=? sc s) (cons (Ranked r (Finish l f t s g)) (from-schoolr sc tail))]
        [else (from-schoolr sc tail)])]))

(: team-rank : String (Listof Finish) -> (Listof (Ranked Finish)))
(define (team-rank sc fs)
  (top-nr 3 (from-schoolr sc (rank-race fs))))

(: sumr : (Listof (Ranked Finish)) -> Integer)
(define (sumr fs)
 (match fs
   ['() 0]
   [(cons (Ranked r _) tail) (+ r (sumr tail))]))

(: team-score (String (Listof Finish) -> (U Integer 'NoScore)))
(define (team-score sc fs)
  (cond
   [(< (length (team-rank sc fs)) 3) 'NoScore]
   [else (sumr (team-rank sc fs))]))  
(check-expect (team-score "Pepper" sample-data) 8)

(: sample-data (Listof Finish))
(define sample-data
  (list
   (Finish "Petersen" "Rhea" 550588 "Pepper" 5)
   (Finish "Kumar" "Jennifer" 552342 "Central" 8)
   (Finish "Jones" "Zelda" 552521 "Pepper" 5)
   (Finish "Rosen" "Marci" 552961 "Pepper" 7)
   (Finish "Anderson" "Pauline" 553188 "West" 7)
   (Finish "Green" "Peggy" 553486 "Central" 5)
   (Finish "Jameson" "Beyonce" 553510 "Mercy" 5)
   (Finish "Jackson" "Patricia" 554045 "West" 5)
   (Finish "Green" "Alexandra" 555005 "Ridgemont" 5)
   (Finish "Williams" "Beyonce" 555783 "Harper" 8)
   (Finish "Brown" "Cecilia" 555877 "West" 5)
   (Finish "Cohen" "Sophia" 557265 "Pepper" 7)
   (Finish "McBride" "Paula" 557313 "Mercy" 8)
   (Finish "Rosen" "Pavithra" 557916 "Central" 7)
   (Finish "Bingham" "Paula" 558498 "Laurel" 7)
   (Finish "McBride" "Margaret" 559266 "Central" 5)
   (Finish "Kumar" "Solange" 559525 "Harper" 5)
   (Finish "Strickland" "Cecilia" 560445 "Pepper" 8)
   (Finish "Craig" "Beyonce" 561214 "Harper" 6)
   (Finish "Cohen" "Melissa" 561588 "Central" 5)
   (Finish "Cohen" "Martha" 561907 "Laurel" 6)
   (Finish "Sierpinski" "Arna" 563289 "Harper" 6)
   (Finish "Green" "Pauline" 563466 "Central" 8)
   (Finish "Bingham" "Sally" 564730 "Mercy" 7)
   (Finish "Jones" "Mona" 564842 "Laurel" 7)
   (Finish "Miller" "Mia" 564866 "Harper" 8)
   (Finish "Rosen" "Selena" 566634 "Harper" 7)
   (Finish "Kershaw" "Alice" 568149 "Harper" 5)
   (Finish "Anderson" "Kathy" 568258 "Southeast" 8)
   (Finish "Sellers" "Mia" 568429 "Ridgemont" 5)
   (Finish "Brown" "Samantha" 568941 "West" 7)
   (Finish "Cameron" "Margaret" 570560 "Central" 7)
   (Finish "Jackson" "Peggy" 571115 "Mercy" 6)
   (Finish "Douglas" "Marcia" 571229 "Southeast" 6)
   (Finish "Bingham" "Elizabeth" 572615 "Mercy" 7)
   (Finish "Mandelbrot" "Alexandra" 573950 "Central" 7)
   (Finish "Douglas" "Jennifer" 574276 "Southeast" 7)
   (Finish "Maclean" "Alice" 575320 "Southeast" 6)
   (Finish "Douglas" "Arna" 575839 "Harper" 7)
   (Finish "Maclean" "Claire" 576470 "West" 5)
   (Finish "Miller" "Rebecca" 577740 "Harper" 8)
   (Finish "White" "Martha" 578099 "Ridgemont" 7)
   (Finish "Ramsey" "Beyonce" 580197 "West" 6)
   (Finish "Kershaw" "Arna" 580497 "Laurel" 6)
   (Finish "Maclean" "Rebecca" 580692 "Harper" 6)
   (Finish "Williams" "Adithi" 580849 "Southeast" 6)
   (Finish "Anderson" "Rhea" 581138 "Pepper" 5)
   (Finish "Miller" "Martha" 581324 "Central" 6)
   (Finish "Niven" "Christine" 581720 "Laurel" 5)
   (Finish "Anderson" "Zetta" 582455 "Pepper" 6)
   (Finish "Mandelbrot" "Mia" 582624 "Southeast" 8)
   (Finish "Hopper" "Marci" 582916 "Pepper" 6)
   (Finish "Bingham" "Kathy" 583331 "Harper" 8)
   (Finish "Brown" "Elizabeth" 584290 "Central" 7)
   (Finish "Patel" "Anne" 584533 "Central" 6)
   (Finish "Sellers" "Esther" 586198 "Harper" 5)
   (Finish "Cameron" "Kathy" 586389 "Ridgemont" 8)
   (Finish "Connery" "Rebecca" 586636 "Ridgemont" 7)
   (Finish "Cohen" "Jennifer" 586732 "Harper" 8)
   (Finish "Johnson" "Zelda" 587009 "West" 8)
   (Finish "Bingham" "Anne" 588437 "Pepper" 6)
   (Finish "Peyton" "Rhea" 589620 "Mercy" 7)
   (Finish "Cameron" "Sara" 590284 "West" 6)
   (Finish "Jones" "Samantha" 591396 "Central" 7)
   (Finish "Jameson" "Marci" 591555 "Pepper" 7)
   (Finish "Mattison" "Esther" 592215 "Mercy" 6)
   (Finish "Bingham" "Caroline" 592892 "Ridgemont" 5)
   (Finish "Douglas" "Pavithra" 595182 "Harper" 8)
   (Finish "Ramsey" "Mia" 595465 "Ridgemont" 8)
   (Finish "Maclean" "Elizabeth" 596543 "Ridgemont" 5)
   (Finish "Maclean" "Mona" 597687 "Laurel" 5)
   (Finish "James" "Melissa" 597920 "Mercy" 5)
   (Finish "Cohen" "Selena" 598903 "Laurel" 7)
   (Finish "Craig" "Melissa" 598977 "West" 6)
   (Finish "Hopper" "Patricia" 599633 "Mercy" 7)
   (Finish "Schultz" "Samantha" 599684 "Mercy" 7)
   (Finish "Douglas" "Rachel" 599728 "Ridgemont" 6)
   (Finish "Watts" "Penelope" 600465 "Southeast" 7)
   (Finish "James" "Mona" 601092 "Southeast" 7)
   (Finish "Shapiro" "Jennifer" 601150 "Ridgemont" 5)
   (Finish "Narayan" "Pavithra" 601491 "Ridgemont" 6)
   (Finish "Williams" "Alice" 602112 "Central" 7)
   (Finish "Craig" "Arna" 602230 "Harper" 6)
   (Finish "Ramsey" "Solange" 602879 "Southeast" 5)
   (Finish "Johnson" "Sophia" 603051 "Laurel" 8)
   (Finish "Kershaw" "Sally" 603332 "Laurel" 6)
   (Finish "Black" "Sally" 603401 "West" 8)
   (Finish "Schultz" "Marcia" 603521 "Harper" 5)
   (Finish "Brown" "Rachel" 603918 "Ridgemont" 5)
   (Finish "Moore" "Elizabeth" 604221 "Laurel" 7)
   (Finish "Watts" "Cecilia" 605602 "Pepper" 7)
   (Finish "Hopkinson" "Zetta" 606148 "West" 5)
   (Finish "McBride" "Selena" 606932 "Harper" 5)
   (Finish "Kumar" "Sophia" 607778 "Ridgemont" 8)
   (Finish "Cohen" "Samantha" 608190 "Pepper" 5)
   (Finish "Miller" "Mona" 608326 "Southeast" 6)
   (Finish "Ballard" "Shreya" 609172 "West" 7)
   (Finish "Bingham" "Rachel" 609177 "Mercy" 8)
   (Finish "Petersen" "Pavithra" 611143 "Laurel" 7)
   (Finish "Mattison" "Rhea" 611204 "Laurel" 7)))

 (test)