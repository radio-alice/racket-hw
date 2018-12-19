#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")


(define-struct Salary
  ([per-period : Exact-Rational]
   [periods-per-year : Integer]
   [min-hours-week : Integer]))

(define-struct Wage
  ([hourly : Exact-Rational]
   [max-hours-week : Integer]))

(define-type Job (U Salary Wage))

; raises error if any given value is negative, otherwise returns given Job
(: job-error : Job -> Job)
(define (job-error j)
  (match j
    [(Salary pp ppy mhw)
     (cond
       [(or (< pp 0) (< ppy 0) (< mhw 0)) (error  "job-error: cannot have a negative value in Job")]
       [(> mhw 80) (error "job-error: working too many hours")]
       [else j])]
    
    [(Wage h mhw)
     (cond
       [(or  (< h 0) (< mhw 0)) (error  "job-error: cannot have a negative value in Job")]
       [(> mhw 80) (error "job-error: working too many hours")]
       [else j])]))
(check-error (job-error (Salary -3 5 5)) "job-error: cannot have a negative value in Job")
(check-error (job-error (Wage -3 5)) "job-error: cannot have a negative value in Job")

; returns annual income given job and hours-worked/week
(: annual-income : Job Integer -> Exact-Rational)
(define (annual-income job hw)
 (local
  {(define j (job-error job))}
   (match j
    [(Salary p pp m)
     (cond
       [(> hw m) (* p pp)]
       [else 0])]
    [(Wage h m)
     (cond
       [(< hw m) (* h hw 50)]
       [else (* m hw 50)])])))
(check-expect (annual-income (Salary 5 5 5) 8) 25)
(check-expect (annual-income (Wage 5 5) 3) 750)

; returns the effective hourly rate for given job, hours-worked/week
(: hourly-rate : Job Integer -> Exact-Rational)
(define (hourly-rate job hw)
 (local
   {(define j (job-error job))}
   (match j
    [(Salary p pp m) (/ (annual-income j hw) (* hw 50))]
    [(Wage h m) h])))
(check-within (hourly-rate (Salary 5 5 5) 10) .05 .001)
(check-expect (hourly-rate (Wage 5 5) 3) 5)
(check-error (hourly-rate (Salary -5 -5 0) 10) "job-error: cannot have a negative value in Job")

; determines whether two given jobs are equal
(: job=? : Job Job -> Boolean)
(define (job=? a b)
  (match (list a b)
    [(list (Salary ppa ppya mhwa) (Salary ppb ppyb mhwb)) (and (= ppa ppb) (= ppya ppyb) (= mhwa mhwb))]
    [(list (Wage ha ma) (Wage hb mb)) (and (= ha hb) (= ma mb))]
    [else #f]))

; determines whether first given job earns more than second
(: earnings>? : Job Job -> Boolean)
(define (earnings>? job-a job-b)
  (local
    {(define a (job-error job-a))
     (define b (job-error job-b))}
    (> (annual-income a
        (match a
          [(Salary pp ppy mhw) (+ 1 mhw)]
          [(Wage h m) m]))
       (annual-income b
         (match b
          [(Salary pp ppy mhw) (+ 1 mhw)]
          [(Wage h m) m])))))
(check-expect (earnings>? (Salary 200 5 5) (Wage 5 3)) #t)


;; Problem 2
(define-type Player (U 'Black 'White))
(define-type PieceType (U 'Pawn 'Bishop 'Knight 'Rook 'Queen 'King))
(define-struct ChessPiece
  ([color : Player]
   [type : PieceType]))

; return number of points given piece is worth
(: points : ChessPiece -> Integer)
(define (points p)
  (match p
    [(ChessPiece _ 'Pawn) 1]
    [(ChessPiece _ 'Bishop) 3]
    [(ChessPiece _ 'Knight) 3]
    [(ChessPiece _ 'Rook) 5]
    [(ChessPiece _ 'Queen) 9]
    [(ChessPiece _ 'King) 0]))
(check-expect (points (ChessPiece 'White 'Queen)) 9)

;; return character representing piece and vice-versa
; return white piece character
(: white-c : ChessPiece -> Char)
(define (white-c p)
 (match p
   [(ChessPiece _ 'Pawn) #\p]
   [(ChessPiece _ 'Bishop) #\b]
   [(ChessPiece _ 'Knight) #\n]
   [(ChessPiece _ 'Rook) #\r]
   [(ChessPiece _ 'Queen) #\q]
   [(ChessPiece _ 'King) #\k]))

; return any piece char
(: piece->char : ChessPiece -> Char)
(define (piece->char p)
  (match p
    [(ChessPiece 'White _) (white-c p)]
    [else (char-upcase (white-c p))]))
(check-expect (piece->char (ChessPiece 'White 'Queen)) #\q)
(check-expect (piece->char (ChessPiece 'Black 'Queen)) #\Q)

; return black piece symbol from char
(: black-p : Char -> PieceType)
(define (black-p c)
  (match c
    [#\P 'Pawn]
    [#\N 'Knight]
    [#\B 'Bishop]
    [#\Q 'Queen]
    [#\K 'King]
    [else (error "char->piece: given character does not match piece")]))
(check-error (black-p #\U) "char->piece: given character does not match piece")

; return piece given char
(: char->piece : Char -> ChessPiece)
(define (char->piece c)
 (cond
   [(char-upper-case? c) (ChessPiece 'Black (black-p c))]
   [else (ChessPiece 'White (black-p (char-upcase c)))]))
(check-expect (char->piece #\q) (ChessPiece 'White 'Queen))
(check-expect (char->piece #\Q) (ChessPiece 'Black 'Queen))

;; Problem 3
(define-type IntTree (U IntNode 'IEmpty))

(define-struct IntNode
  ([val   : Integer]
   [left  : IntTree]
   [right : IntTree]))

(define-type StringTree (U StringNode 'SEmpty))

(define-struct StringNode
  ([val   : String]
   [left  : StringTree]
   [right : StringTree]))

; return mirror (l/r) of tree
(: mirror : IntTree -> IntTree)
(define (mirror t)
  (match t
    ['IEmpty t]
    [(IntNode v l r) (IntNode v (mirror r) (mirror l))]))
(check-expect (mirror (IntNode 1 (IntNode 2 'IEmpty 'IEmpty)
                (IntNode 3 (IntNode 4 'IEmpty 'IEmpty) (IntNode 5 'IEmpty 'IEmpty))))
               (IntNode 1 (IntNode 3 (IntNode 5 'IEmpty 'IEmpty)
                 (IntNode 4 'IEmpty 'IEmpty)) (IntNode 2 'IEmpty 'IEmpty)))

; given tree of ints, return matching tree of strings
(: int-tree->string-tree : IntTree -> StringTree)
(define (int-tree->string-tree t)
  (match t
    ['IEmpty 'SEmpty]
    [(IntNode v l r) (StringNode (number->string v)
                    (int-tree->string-tree l) (int-tree->string-tree r))]))
(check-expect (int-tree->string-tree (IntNode 1 (IntNode 2 'IEmpty 'IEmpty)
                (IntNode 3 (IntNode 4 'IEmpty 'IEmpty) (IntNode 5 'IEmpty 'IEmpty))))
               (StringNode "1" (StringNode "2" 'SEmpty 'SEmpty)
                (StringNode "3" (StringNode "4" 'SEmpty 'SEmpty) (StringNode "5" 'SEmpty 'SEmpty))))

; produce string representing right edge of string tree
(: right-edge : StringTree -> String)
(define (right-edge t)
  (match t
    ['SEmpty ""]
    [(StringNode s _ r) (string-append s (right-edge r))]))
(check-expect (right-edge (StringNode "1" (StringNode "2" 'SEmpty 'SEmpty)
                (StringNode "3" (StringNode "4" 'SEmpty 'SEmpty) (StringNode "5" 'SEmpty 'SEmpty))))
                  "135")

;; Problem 4
(define-type 3Tree (U 3Node '3Empty))

(define-struct 3Node
  ([root : Integer]
   [lsub : 3Tree]
   [msub : 3Tree]
   [rsub : 3Tree]))

; returns number of nodes in tree
(: num-nodes : 3Tree -> Integer)
(define (num-nodes t)
  (match t
    ['3Empty 0]
    [(3Node r ls ms rs) (+ 1 (num-nodes ls) (num-nodes ms) (num-nodes rs))]))
(check-expect (num-nodes 3Tree-test) 10)

; returns sum of nodes in tree
(: sum-nodes : 3Tree -> Integer)
(define (sum-nodes t)
  (match t
    ['3Empty 0]
    [(3Node r ls ms rs) (+ r (sum-nodes ls) (sum-nodes ms) (sum-nodes rs))]))
(check-expect (sum-nodes 3Tree-test) 45)

; returns length of longest branch of tree
(: height : 3Tree -> Integer)
(define (height t)
  (match t
    ['3Empty 0]
    [(3Node r ls ms rs) (+ 1 (max (height ls) (height ms) (height rs)))]))
(check-expect (height 3Tree-test) 4)

; returns whether tree contains given int
(: contains? : 3Tree Integer -> Boolean)
(define (contains? t i)
  (match t
    ['3Empty #f]
    [(3Node r ls ms rs) (or (= r i) (contains? ls i)  (contains? ms i) (contains? rs i))]))
(check-expect (contains? 3Tree-test 7) #t)
(check-expect (contains? 3Tree-test 10) #f)

; returns leftmost item in tree
(: leftmost : 3Tree -> (U Integer 'None))
(define (leftmost t)
  (match t
    ['3Empty 'None]
    [(3Node r '3Empty _ _) r]
    [(3Node r ls _ _) (leftmost ls)]))
(check-expect (leftmost 3Tree-test) 3)

; returns branch with longest length
(: longestB : 3Tree -> 3Tree)
(define (longestB t)
  (match t
    ['3Empty '3Empty]
    [(3Node r ls ms rs)
     (cond
       [(and (>= (height ls) (height ms)) (>= (height ls) (height rs))) ls]
       [(>= (height ms) (height rs)) ms]
       [else rs])]))
; returns item farthest from root
(: farthest-item : 3Tree -> (U Integer 'None))
(define (farthest-item t)
  (match t
    ['3Empty 'None]
    [(3Node r '3Empty '3Empty '3Empty) r]
    [(3Node r ls ms rs) (farthest-item (longestB t))]))
(check-expect (farthest-item 3Tree-test) 5)

; returns new tree with each int doubled
(: double : 3Tree -> 3Tree)
(define (double t)
  (match t
    ['3Empty '3Empty]
    [(3Node r ls ms rs) (3Node (* 2 r) (double ls) (double ms) (double rs))]))
(check-expect (double 3Tree-test)
      (3Node 2
       (3Node 4
              (3Node 6 '3Empty '3Empty '3Empty)
              (3Node 8 '3Empty '3Empty '3Empty)
              '3Empty)
       (3Node 16
              '3Empty
              (3Node 14
                     (3Node 10 '3Empty '3Empty '3Empty)
                     '3Empty
                     (3Node 12 '3Empty '3Empty '3Empty))
              '3Empty)
       (3Node 18
              '3Empty
              '3Empty
              (3Node 0 '3Empty '3Empty '3Empty))))

(define 3Tree-test : 3Tree
  (3Node 1
       (3Node 2
              (3Node 3 '3Empty '3Empty '3Empty)
              (3Node 4 '3Empty '3Empty '3Empty)
              '3Empty)
       (3Node 8
              '3Empty
              (3Node 7
                     (3Node 5 '3Empty '3Empty '3Empty)
                     '3Empty
                     (3Node 6 '3Empty '3Empty '3Empty))
              '3Empty)
       (3Node 9
              '3Empty
              '3Empty
              (3Node 0 '3Empty '3Empty '3Empty))))
(test)
