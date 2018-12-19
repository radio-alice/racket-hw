#lang typed/racket

(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

;; data definitions
(define-struct (Some a)
  ([value : a]))

(define-type (Optional a)
  (U 'None (Some a)))

(define-type Stone
  (U 'black 'white))

(define-struct LogicalLoc
  ([col : Integer]
   [row : Integer]))
   
(define-struct Go
  ([dimension : Integer]
   [black-stones : (Listof LogicalLoc)]
   [white-stones : (Listof LogicalLoc)]
   [next-to-play : Stone]))

(define-struct PhysicalLoc
  ([x-offset-from-left : Integer]
   [y-offset-from-top  : Integer]))

(define-struct BoardSpec
  ([background-color : Image-Color]
   [cell-size-pixels : Integer]
   [margin-pixels : Integer]
   [stone-radius-pixels : Integer]))

(define-struct World
  ([spec : BoardSpec]
   [game : Go]
   [status-message : String]))

;; test definitions
(define test-LL1
  (LogicalLoc 9 9))
(define test-LL2
  (LogicalLoc 0 18))
(define test-LL3
  (LogicalLoc 18 18))
(define test-LL4
  (LogicalLoc 30 30))

(define test-BS1
  (BoardSpec 'gray 24 20 8))

(define test-PL1
  (PhysicalLoc 236 236))
(define test-PL1-2
  (PhysicalLoc 228 244))

(define test-PL2
  (PhysicalLoc 20 20))
(define test-PL2-2
  (PhysicalLoc 28 13))
(define test-PL3
  (PhysicalLoc 452 20))
(define test-PL3-2
  (PhysicalLoc 451 25))
(define test-PL-None1
  (PhysicalLoc 29 32))
(define test-PL-None2
  (PhysicalLoc 452 29))

;; returns physical loc given logical loc, number of cells, board spec
(: logical->physical : LogicalLoc Integer BoardSpec -> PhysicalLoc)
(define (logical->physical LL n bs)
  (match* (LL bs)
    [((LogicalLoc c r) (BoardSpec _ cell margin _))
     (PhysicalLoc (+ margin (* c cell)) (+ margin (* (- (sub1 n) r) cell)))]))
(check-expect (logical->physical test-LL1 19 test-BS1) test-PL1)
(check-expect (logical->physical test-LL2 19 test-BS1) test-PL2)
(check-expect (logical->physical test-LL3 19 test-BS1) test-PL3)

;; return logical loc given physical, num cells, board spec
(: physical->logical : PhysicalLoc Integer BoardSpec -> (Optional LogicalLoc))
(define (physical->logical PL n bs)
  (match* (PL bs)
    [((PhysicalLoc x y) (BoardSpec _ cell margin stone))
     (local
       {(: rem : Integer -> Integer)
        (define (rem a) (remainder (- a margin) cell))}
       (local
       {(: convert : Integer -> Integer)
        (define (convert a) (round (/ (- a margin) cell)))}
       (cond
         [(and (> (- x stone) 0) (< x (+ (* (sub1 n) cell) margin stone))
               (or (>= stone (rem x)) (>= stone (- cell (rem x))))
               (> (- y stone) 0) (< y (+ (* (sub1 n) cell) margin stone))
               (or (>= stone (rem y)) (>= stone (- cell (rem y)))))
                   (Some (LogicalLoc (convert x) (- (sub1 n) (convert y))))]
         [else 'None])))]))
(check-expect (physical->logical test-PL1 19 test-BS1) (Some test-LL1))
(check-expect (physical->logical test-PL1-2 19 test-BS1) (Some test-LL1))
(check-expect (physical->logical test-PL2 19 test-BS1) (Some test-LL2))
(check-expect (physical->logical test-PL2-2 19 test-BS1) (Some test-LL2))
(check-expect (physical->logical test-PL3 19 test-BS1) (Some test-LL3))
(check-expect (physical->logical test-PL3-2 19 test-BS1) (Some test-LL3))
(check-expect (physical->logical test-PL-None1 19 test-BS1) 'None)
(check-expect (physical->logical test-PL-None2 19 test-BS1) 'None)

;; match int to character
(: int->char : Integer -> Char)
(define (int->char n)
  (local {(define a (remainder n 25))}
    (match a
      [0 #\A] [1 #\B] [2 #\C] [3 #\D]
      [4 #\E] [5 #\F] [6 #\G] [7 #\H]
      [8 #\J] [9 #\K] [10 #\L] [11 #\M]
      [12 #\N] [13 #\O] [14 #\P] [15 #\Q]
      [16 #\R] [17 #\S] [18 #\T] [19 #\U]
      [20 #\V] [21 #\W] [22 #\X] [23 #\Y] [24 #\Z])))
(check-expect (int->char 22) #\X)
(check-expect (int->char 27) #\C)

;; return string representing LogicalLoc
(: logical->string : LogicalLoc -> String)
(define (logical->string LL)
  (match LL
    [(LogicalLoc x y) (string-append
                       (make-string (add1 (quotient x 25)) (int->char x))
                       (number->string (add1 y)))]))
(check-expect (logical->string test-LL1) "K10")
(check-expect (logical->string test-LL4) "FF31")

;; return whether two LLs are equal
(: LL=? : LogicalLoc LogicalLoc -> Boolean)
(define (LL=? l1 l2)
  (match* (l1 l2)
    [((LogicalLoc x1 y1) (LogicalLoc x2 y2))
     (if (and (= x1 x2) (= y1 y2)) #t #f)]))
(check-expect (LL=? test-LL1 test-LL1) #t)
(check-expect (LL=? test-LL1 test-LL2) #f)

;; return whether given LL is present in list
(: memberLL : LogicalLoc (Listof LogicalLoc) -> Boolean)
(define (memberLL LL LLs)
  (match LLs
    ['() #f]
    [(cons h t) (or (LL=? LL h) (memberLL LL t))]))
(check-expect (memberLL test-LL1 (list test-LL2 test-LL3 test-LL4)) #f)
(check-expect (memberLL test-LL1 (list test-LL1 test-LL2 test-LL3 test-LL4)) #t)

;; returns the stone at the specified location on the board, or indicates it is unoccupied
(: board-ref : Go LogicalLoc -> (Optional Stone))
(define (board-ref go LL)
  (match go
    [(Go _ blacks whites _)
     (cond
       [(memberLL LL blacks) (Some 'black)]
       [(memberLL LL whites) (Some 'white)]
       [else 'None])]))

;; If the named location is unoccupied, put the stone there and advance the player, and
;; return Some Go struct. Return 'None if the location is already occupied.
;; Raise an error if the stone to be placed does not match the color of the
;; player whose turn is indicated by the Go struct.
(: put-stone-at : LogicalLoc Stone Go -> (Optional Go))
(define (put-stone-at LL stone go)
  (match (board-ref go LL)
    ['None
     (match go
       [(Go dim blacks whites 'black) (if (symbol=? stone 'black)
                                          (Some (Go dim (cons LL blacks) whites 'white))
                                          (error "put-stone-at: not your turn"))]
       [(Go dim blacks whites 'white) (if (symbol=? stone 'white)
                                          (Some (Go dim blacks (cons LL whites) 'black))
                                          (error "put-stone-at: not your turn"))])]
    [_ 'None]))
        
;; returns whether board-spec is valid
(: valid-board-spec? : BoardSpec -> Boolean)
(define (valid-board-spec? BS)
  (match BS
    [(BoardSpec _ cell margin stone)
     (and (> stone 0) (> (/ cell 2) stone)
          (> margin stone))]))

;; draw grid of given dimension and cell size
(: grid : Integer Integer -> Image)
(define (grid n cell)
  (foldr above empty-image (make-list (sub1 n) 
     (foldr beside empty-image (make-list (sub1 n) (square cell 'outline 'black))))))

;; draw board given dimension and board-spec
(: draw-board : Integer BoardSpec String -> Image)
(define (draw-board n BS msg)
  (match BS
    [(BoardSpec color cell margin _)
     (above (overlay
      (grid n cell)
      (square (+ (* 2 margin) (* (sub1 n) cell)) 'solid color)) (text msg 12 "black"))]))

;; create image of chars from size of board
(: label-bottom : Integer Integer -> Image)
(define (label-bottom dim cell)
  (match dim
    [0 empty-image]
    [x (beside (text (string (int->char (sub1 x))) 12 "black")
               (square (- cell 8) 0 'white)
               (label-bottom (sub1 x) cell))]))

;; create image of ints from dimension
(: label-side : Integer Integer -> Image)
(define (label-side dim cell)
  (match dim
    [0 empty-image]
    [x (above (text (number->string x) 12 "black")
              (square (- cell 12) 0 'white)
               (label-side (sub1 x) cell))]))

;; draw stones
(: draw-stones : (Listof PhysicalLoc) Color Integer Image -> Image)
(define (draw-stones PLs c r img)
  (match PLs
    ['() img]
    [(cons (PhysicalLoc x y) t)
     (overlay/align/offset "left" "top" (circle r 'solid c)
                           (+ (* -1 x) r) (+ r (* -1 y)) (draw-stones t c r img))]))

;; convert list of LL->PL
(: LLs->PLs : (Listof LogicalLoc) Integer BoardSpec -> (Listof PhysicalLoc))
(define (LLs->PLs LLs dim BS)
  (match LLs
    ['() '()]
    [(cons h t) (cons (logical->physical h dim BS) (LLs->PLs t dim BS))]))
(check-expect (LLs->PLs (list test-LL1 test-LL2 test-LL3) 19 test-BS1) (list test-PL1 test-PL2 test-PL3))

;; draws world
(: draw : World -> Image)
(define (draw w)
  (match w
    [(World BS go msg)
     (match* (go BS)
       [((Go dim blacks whites next) (BoardSpec col cell margin stone))
        (beside (above (draw-stones (LLs->PLs blacks dim BS) (color 0 0 0) stone
                     (draw-stones (LLs->PLs whites dim BS) (color 255 255 255) stone
        (draw-board dim BS msg))) (label-bottom dim cell)) (label-side dim cell))])]))

;; turns clicks into stone placement
(: react-to-click : World Integer Integer Mouse-Event -> World)
(define (react-to-click w x y m)
  (match m
    ["button-down"
     (match w
       [(World BS go msg)
        (match go
          [(Go dim _ _ next)
           (local {(define a (physical->logical (PhysicalLoc x y) dim BS))}
             (match a
               [(Some (LogicalLoc Lx Ly))
                (local {(define b (put-stone-at (LogicalLoc Lx Ly) next go))}
                  (match b
                    [(Some (Go dim1 bl wh next1)) (World BS (Go dim1 bl wh next1) (string-append (symbol->string next1) "'s turn"))]
                    ['None w]))]
               ['None w]))])])]
    [_ w]))

;; handles passing
(: react-to-p : World String -> World)
(define (react-to-p w k)
  (match k
    ["p" (match w
           [(World BS go msg)
            (match go
              [(Go a b c 'white) (World BS (Go a b c 'black) "black's turn")]
              [(Go a b c 'black) (World BS (Go a b c 'white) "white's turn")])])]
    [_ w]))

;; initiate empty board
(: empty-Go : Integer -> Go)
(define (empty-Go n)
  (Go n '() '() 'black))

;; provides board of given dimension, initiates game
(: play : Integer BoardSpec -> World)
(define (play n BS)
  (cond
    [(valid-board-spec? BS)
     (cond
       [(> n 2)
        (big-bang (World BS (empty-Go n) "black's turn") : World
          [to-draw draw]
          [on-mouse react-to-click]
          [on-key react-to-p])]
       [else (error "play: dimension too small")])]
    [else (error "play: invalid boardspec")]))
  

(test)