#lang typed/racket

(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

(define-struct Date
  ([month : Integer]
   [day   : Integer]
   [year  : Integer]))

(define-type Day
  (U 'Sun 'Mon 'Tue 'Wed 'Thu 'Fri 'Sat))

; return double digit string
(: stringify : Integer -> String)
(define (stringify n)
  (cond
    [(< n 10) (string-append "0" (number->string n))]
    [else (number->string n)]))

;; display the date in "MM/DD/YYYY" format
(: format : Date -> String)
(define (format date)
  (match date
    [(Date m d y) (string-append
                   (stringify m) "/"
                   (stringify d) "/"
                   (number->string y))]))
(check-expect (format (Date 10 13 2008)) "10/13/2008")
(check-expect (format (Date 1 1 2008)) "01/01/2008")

;; report the number of days given the month and the year, in that order
(: days-in-month : Integer Integer -> Integer)
(define (days-in-month m y)
  (cond
    [(or (= 1 m) (= 3 m) (= 5 m) (= 7 m) (= 8 m) (= 10 m) (= 12 m)) 31]
    [(or (= 4 m) (= 6 m) (= 9 m) (= 11 m)) 30]
    [(and (= m 2) (= 0 (remainder y 4))) 29]
    [else 28]))
(check-expect (days-in-month 1 2000) 31)
(check-expect (days-in-month 4 2000) 30)
(check-expect (days-in-month 2 2000) 29)
(check-expect (days-in-month 2 2001) 28)

;; a date is valid if it is not past the end of the month (e.g., Feb 30 or Apr 33)
;; and it is between 1900 and 2099 inclusive
(: valid? : Date -> Boolean)
(define (valid? date)
  (match date
    [(Date m d y)
     (cond
       [(and (<= d (days-in-month m y))
             (>= y 1900)
             (<= y 2099)) #t]
       [else #f])]))
(check-expect (valid? (Date 10 13 2008)) #t)
(check-expect (valid? (Date 10 40 2008)) #f)

;; Is the first date before the second?
(: before? : Date Date -> Boolean)
(define (before? a b)
  (match* (a b)
    [((Date ma da ya) (Date mb db yb))
     (cond
       [(> ya yb) #f]
       [(and (= ya yb) (> ma mb)) #f]
       [(and (= ya yb) (= ma mb) (>= da db)) #f]
       [else #t])]))
(check-expect (before? (Date 10 10 2010) (Date 10 9 2010)) #f)
(check-expect (before? (Date 10 10 2010) (Date 10 10 2010)) #f)
(check-expect (before? (Date 10 10 2010) (Date 10 11 2010)) #t)


;; Is the first date after the second?
(: after? : Date Date -> Boolean)
(define (after? a b)
  (match* (a b)
    [((Date ma da ya) (Date mb db yb))
     (cond
       [(and (= ma mb) (= da db) (= ya yb)) #f]
       [else (not (before? a b))])]))
(check-expect (after? (Date 10 10 2010) (Date 10 9 2010)) #t)
(check-expect (after? (Date 10 10 2010) (Date 10 10 2010)) #f)
(check-expect (after? (Date 10 10 2010) (Date 10 11 2010)) #f)


;; Is the given date the last date of the month?
(: last-of-month? : Date -> Boolean)
(define (last-of-month? date)
  (match date
    [(Date m d y) (= d (days-in-month m y))]))
(check-expect (last-of-month? (Date 10 31 2018)) #t)
(check-expect (last-of-month? (Date 10 30 2018)) #f)

;; Given a date, return the day immediately after.
(: tomorrow : Date -> Date)
(define (tomorrow date)
  (match date
    [(Date 12 31 y) (Date 1 1 (+ 1 y))]
    [(Date m d y)
     (cond
       [(last-of-month? date) (Date (+ 1 m) 1 y)]
       [else (Date m (+ 1 d) y)])]))
(check-expect (tomorrow (Date 10 31 2010)) (Date 11 1 2010))
(check-expect (tomorrow (Date 12 31 2010)) (Date 1 1 2011))

;; Given a date, return the day immediately before.
(: yesterday : Date -> Date)
(define (yesterday date)
  (match date
    [(Date 1 1 y) (Date 12 31 (- y 1))]
    [(Date m 1 y) (Date (- m 1) (days-in-month (- m 1) y) y)]
    [(Date m d y) (Date m (- d 1) y)]))

(check-expect (yesterday (Date 11 1 2010)) (Date 10 31 2010))
(check-expect (yesterday (Date 1 1 2010)) (Date 12 31 2009))


;; Given a date, advance that many days on the calendar.
(: add-days : Integer Date -> Date)
(define (add-days n date)
  (cond
    [(= 0 n) date]
    [(> n 0) (add-days (- n 1) (tomorrow date))]
    [(< n 0) (add-days (+ n 1) (yesterday date))]))
(check-expect (add-days 370 (Date 11 1 2010)) (Date 11 6 2011))
(check-expect (add-days 31 (Date 12 1 2010)) (Date 1 1 2011))
(check-expect (add-days -10 (Date 1 1 2010)) (Date 12 22 2009))

; return list of dates plus the next day in month
(: add-next : (Listof Date) -> (Listof Date))
(define (add-next dates)
  (match dates
    ['() '()]
    [(cons (Date m d y) tail)
     (cond
       [(= d 1) dates]
       [else (add-next (cons (Date m (- d 1) y) dates))])]))
;; Return the whole month as a list of dates in ascending order, given month and year.
(: whole-month : Integer Integer -> (Listof Date))
(define (whole-month m y)
  (add-next (list (Date m (days-in-month m y) y))))
(check-expect (whole-month 10 2000)
 (list
 (Date 10 1 2000)
 (Date 10 2 2000)
 (Date 10 3 2000)
 (Date 10 4 2000)
 (Date 10 5 2000)
 (Date 10 6 2000)
 (Date 10 7 2000)
 (Date 10 8 2000)
 (Date 10 9 2000)
 (Date 10 10 2000)
 (Date 10 11 2000)
 (Date 10 12 2000)
 (Date 10 13 2000)
 (Date 10 14 2000)
 (Date 10 15 2000)
 (Date 10 16 2000)
 (Date 10 17 2000)
 (Date 10 18 2000)
 (Date 10 19 2000)
 (Date 10 20 2000)
 (Date 10 21 2000)
 (Date 10 22 2000)
 (Date 10 23 2000)
 (Date 10 24 2000)
 (Date 10 25 2000)
 (Date 10 26 2000)
 (Date 10 27 2000)
 (Date 10 28 2000)
 (Date 10 29 2000)
 (Date 10 30 2000)
 (Date 10 31 2000)))

; return month adjustment
(: monthAdjust : Date -> Integer)
(define (monthAdjust date)
  (match date
    [(Date 1 _ y) (if (= 0 (remainder y 4)) 0 1)]
    [(Date 2 _ y) (if (= 0 (remainder y 4)) 3 4)]
    [(Date 3 _ _) 4]
    [(Date 4 _ _) 0]
    [(Date 5 _ _) 2]
    [(Date 6 _ _) 5]
    [(Date 7 _ _) 0]
    [(Date 8 _ _) 3]
    [(Date 9 _ _) 6]
    [(Date 10 _ _) 1]
    [(Date 11 _ _) 4]
    [(Date 12 _ _) 6]))
(check-expect (monthAdjust (Date 1 2 2004)) 0)
(check-expect (monthAdjust (Date 1 2 2003)) 1)

; return day given int representing DoW
(: int->day : Integer -> Day)
(define (int->day w)
  (match w
    [0 'Sun]
    [1 'Mon]
    [2 'Tue]
    [3 'Wed]
    [4 'Thu]
    [5 'Fri]
    [6 'Sat]))
(check-expect (int->day 1) 'Mon)

; return int representing day
(: dowInt : Date -> Integer)
(define (dowInt date)
  (match date
    [(Date m d y)
       (remainder (+ (- y 1900) (monthAdjust date) d (floor (/ y 4))) 7)]))
(check-expect (dowInt (Date 5 3 1997)) 6)
(check-expect (dowInt (Date 10 31 2018)) 3)

;; Compute the day of the week for the given date. The formula is below.
(: dow : Date -> Day)
(define (dow date)
  (int->day (dowInt date)))
(check-expect (dow (Date 5 3 1997)) 'Sat)
(check-expect (dow (Date 10 31 2018)) 'Wed)

; return list of weekends from given list of dates
(: filter-weekends : (Listof Date) -> (Listof Date))
(define (filter-weekends dates)
  (match dates
    ['() '()]
    [(cons h t) (if (or (symbol=? (dow h) 'Sat)
                        (symbol=? (dow h) 'Sun))
                    (cons h (filter-weekends t))
                    (filter-weekends t))]))

;; Return the Saturdays and Sundays in the given month and year, in ascending order.
(: weekends : Integer Integer -> (Listof Date))
(define (weekends m y)
  (filter-weekends (whole-month m y)))
(check-expect (weekends 10 2018) (list (Date 10 6 2018)
                                   (Date 10 7 2018)
                                   (Date 10 13 2018)
                                   (Date 10 14 2018)
                                   (Date 10 20 2018)
                                   (Date 10 21 2018)
                                   (Date 10 27 2018)
                                   (Date 10 28 2018)))
; return last monday of given month
(: last-mon : (Listof Date) -> Date)
(define (last-mon month)
  (match month
    [(cons (Date m d y) t)
     (if (and (symbol=? (dow (Date m d y)) 'Mon)
                (< (- (days-in-month m y) d) 7))
         (Date m d y) (last-mon t))]))
(check-expect (last-mon (whole-month 10 2018)) (Date 10 29 2018))

; return first monday of given month
(: first-mon : (Listof Date) -> Date)
(define (first-mon month)
  (match month
    [(cons (Date m d y) t)
     (if (and (symbol=? (dow (Date m d y)) 'Mon) (< d 8))
         (Date m d y) (first-mon t))]))
(check-expect (first-mon (whole-month 11 2018)) (Date 11 5 2018))

; return memorial day given year
(: memorial-day : Integer -> Date)
(define (memorial-day y)
  (last-mon (whole-month 5 y)))
(check-expect (memorial-day 2018) (Date 5 28 2018))

; return labor day given year
(: labor-day : Integer -> Date)
(define (labor-day y)
  (first-mon (whole-month 9 y)))
(check-expect (labor-day 2018) (Date 9 3 2018))

; return fourth thursday of given month
(: fourth-thurs : (Listof Date) -> Date)
(define (fourth-thurs month)
  (match month
    [(cons (Date m d y) t)
     (if (and (symbol=? (dow (Date m d y)) 'Thu) (< d 29) (> d 21))
         (Date m d y) (fourth-thurs t))]))
(check-expect (fourth-thurs (whole-month 11 2018)) (Date 11 22 2018))

; return date of thanksgiving given year
(: thanksgiving : Integer -> Date)
(define (thanksgiving y)
  (fourth-thurs (whole-month 11 y)))
(check-expect (thanksgiving 2018) (Date 11 22 2018))

;; ALL CALENDAR IMAGE STUFF BELOW
; take in date and produce square image
(: box : Date -> Image)
(define (box date)
  (match date
    [(Date m d y) (overlay (text (number->string d) 12 'white)
                                 (square 40 'outline 'white))]))

; return list of first n items of list
(: take : (All (a) Integer (Listof a) -> (Listof a)))
(define (take n as)
  (match as
    ['() '()]
    [(cons h t) (if (> n 0)
                    (cons h (take (sub1 n) t))
                    '())]))
(check-expect (take 4 (list 1 2 3 4 5 6 7)) (list 1 2 3 4))

; remove first n items from list
(: drop : (All (a) Integer (Listof a) -> (Listof a)))
(define (drop n as)
  (match as
    ['() '()]
    [(cons h t) (if (> n 1)
                    (drop (sub1 n) t) t)]))
(check-expect (drop 4 (list 1 2 3 4 5 6 7)) (list 5 6 7))

; split month into 5 weeks
(: split-month : (Listof Date) Integer -> (Listof (Listof Date)))
(define (split-month dates blanks)
  (match dates
    ['() '()]
    [_
     (cond
       [(> blanks 0) (split-month (cons (Date 0 0 0) dates) (sub1 blanks))]
       [else (cons (take 7 dates) (split-month (drop 7 dates) 0))])]))

; take in list of dates and produce row for calender
(: boxes : (Listof Date) ->  Image)
(define (boxes dates)
  (match dates
    ['() empty-image]
    [(cons (Date m d y) t)
     (cond
       [(= d 0) (beside (square 40 'outline 'white) (boxes t))]
       [else (beside (box (Date m d y)) (boxes t))])]))
 
; arrange cal rows into grid
(: arrange : (Listof (Listof Date)) -> Image)
(define (arrange dates)
  (match dates
    ['() empty-image]
    [(cons h t) (above/align "left" (boxes h) (arrange t))]))

; day of week labels
(define dow-labels
  (text "Sun Mon Tue Wed Thu Fri Sat" 20 'white))
; produce image of calendar given month and year
(: calendar : Integer Integer -> Image)
(define (calendar m y)
  (above dow-labels (arrange (split-month (whole-month m y) (dowInt (Date m 1 y))))))


(test)