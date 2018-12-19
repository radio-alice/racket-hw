#lang typed/racket

(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

;; capturing doesn't work, didn't have time to figure out why
;; remove-stones! and identify-chain seem to be working right,
;; so I think the issue is in how they're called from apply-move
;; might fix over the next few days 

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

(define-type Board
  (Vectorof (Vectorof (Optional Stone))))

(define-struct Go
  ([board : Board]
   [next-to-play : Stone]
   [history : (Listof Board)]))

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

;; returns the stone at the specified location on the board, or indicates it is unoccupied
(: bd-ref : Board LogicalLoc -> (Optional Stone))
(define (bd-ref board LL)
  (match LL
    [(LogicalLoc x y) (vector-ref (vector-ref board x) y)]))

;; passes board from given Go to bd-ref
(: board-ref : Go LogicalLoc -> (Optional Stone))
(define (board-ref go LL)
  (match go 
    [(Go board _ _) (bd-ref board LL)]))

;; modify board to store given stone at given loc
(: bd-set! : Board LogicalLoc (Optional Stone) -> Void)
(define (bd-set! board LL stone)
  (match LL [(LogicalLoc x y)
             (begin (vector-set! (vector-ref board x) y stone))]))

;; modify given go to store given stone at given loc
(: board-set! : Go LogicalLoc (Optional Stone) -> Void)
(define (board-set! go LL stone)
  (match go
    [(Go board _ _)
     (bd-set! board LL stone)]))

;; return opposite stone
(: switch-turn : Stone -> Stone)
(define (switch-turn stone)
  (match stone
    ['white 'black]
    ['black 'white]))

;; If the named location is unoccupied, put the stone there and advance the player, and
;; return Some Go struct. Return 'None if the location is already occupied.
(: put-stone-at : LogicalLoc Stone Go -> (Optional Go))
(define (put-stone-at LL stone go)
  (match (board-ref go LL)
    ['None
     (match go
       [(Go n stone h) (begin (board-set! go LL (Some stone))
                              (Some (Go n (switch-turn stone) h)))]
       [_ 'None])]))

;; returns whether two vectors of optional stones are equal
(: col=? : (Vectorof (Optional Stone)) (Vectorof (Optional Stone)) -> Boolean)
(define (col=? col1 col2)
  (local
    {(define len1 (vector-length col1))
     (: lp1 : Integer -> Boolean)
     (define (lp1 j)
       (cond
         [(= j len1) #t]
         [else (local
                 {(define stone1 (vector-ref col1 j))
                  (define stone2 (vector-ref col2 j))}
                 (match* (stone1 stone2)
                   [('None 'None) (lp1 (add1 j))]
                   [((Some s1) (Some s2)) (and (symbol=? s1 s2) (lp1 (add1 j)))]
                   [((Some s1) 'None) #f]
                   [('None (Some s2)) #f]))]))}
    (lp1 0)))
                   
;; returns whether two given boards are equal
(: board=? : Board Board -> Boolean)
(define (board=? b1 b2)
  (local
    {(define len (vector-length b1))
     (: lp : Integer -> Boolean)
     (define (lp i)
       (cond
         [(= i len) #t]
         [else (and (col=? (vector-ref b1 i) (vector-ref b2 i)) (lp (add1 i)))]))}
     (if (= len (vector-length b2))
         (lp 0)
         #f)))

;; given a history and a board, check if board is new position
(: check-hist : (Listof Board) Board -> Boolean)
(define (check-hist hist board)
  (match hist
    ['() #t]
    [(cons h t) (and (not (board=? board h)) (check-hist t board))]))

;; returns bool indicating if attempted move is legal
(: legal-move? : Go LogicalLoc -> Boolean)
(define (legal-move? go LL)
  (match (board-ref go LL)
    ['None
      (match go
        [(Go board next hist)
         (local {(define bcopy (board-copy board))}
           (begin (bd-set! bcopy LL (Some next)) (check-hist hist bcopy)))])]
      [_ #f]))

;; returns whether board-spec is valid
(: valid-board-spec? : BoardSpec -> Boolean)
(define (valid-board-spec? BS)
  (match BS
    [(BoardSpec _ cell margin stone)
     (and (> stone 0) (> (/ cell 2) stone)
          (> margin stone))]))

;; creates copy of current board
(: board-copy : Board -> Board)
(define (board-copy board)
  (local
    {(define len (vector-length board))
     (define new-vec (empty-board len))
     (: lp : Integer -> Board)
     (define (lp i)
       (cond
         [(= i len) new-vec]
         [else (begin (vector-set! new-vec i (vector-copy (vector-ref board i)))
                      (lp (add1 i)))]))}
          (lp 0)))

;; are the given stones equal
(: stone=? : (Optional Stone) (Optional Stone) -> Boolean)
(define (stone=? stone1 stone2)
  (match* (stone1 stone2)
    [((Some a) (Some b)) (symbol=? a b)]
    [(_ _) #f]))

;; is the given optional stone empty?
(: stone-empty? : (Optional Stone) -> Boolean)
(define (stone-empty? s)
  (match s
    ['None #t]
    [_ #f]))

;; return true if a given location on given board has any liberties
(: liberties? : LogicalLoc Board -> Boolean)
(define (liberties? LL board)
  (local {(define pos (bd-ref board LL))
          (define dim (vector-length board))
          (: stone-at : LogicalLoc -> (Optional Stone))
          (define (stone-at LL1) (bd-ref board LL1))}
    (match* (pos LL)
      [('None _) #t]
      [((Some stone) (LogicalLoc x y))
       (or (cond
             [(= 0 x) (stone-empty? (stone-at (LogicalLoc (add1 x) y)))]
             [(= dim (sub1 x)) (stone-empty? (stone-at (LogicalLoc (sub1 x) y)))]
             [else (or (stone-empty? (stone-at (LogicalLoc (sub1 x) y)))
                       (stone-empty? (stone-at (LogicalLoc (add1 x) y))))])
           (cond
            [(= 0 y) (stone-empty? (stone-at (LogicalLoc x (add1 y))))]
            [(= dim (sub1 y)) (stone-empty? (stone-at (LogicalLoc x (sub1 y))))]
            [else (or (stone-empty? (stone-at (LogicalLoc x (add1 y))))
                      (stone-empty? (stone-at (LogicalLoc x (sub1 y)))))]))])))

;; return list of neighbors
(: neighbors : LogicalLoc Board -> (Listof LogicalLoc))
(define (neighbors LL board)
  (local {(define pos (bd-ref board LL))
          (define dim (vector-length board))}
    (match* (pos LL)
      [('None _) '()]
      [((Some stone) (LogicalLoc x y))
       (append (cond
                 [(= 0 x) (list (LogicalLoc (add1 x) y))]
                 [(= dim (sub1 x)) (list (LogicalLoc (sub1 x) y))]
                 [else (append (list (LogicalLoc (add1 x) y))
                               (list (LogicalLoc (sub1 x) y)))])
               (cond
                 [(= 0 y) (list (LogicalLoc x (add1 y)))]
                 [(= dim (sub1 y)) (list (LogicalLoc x (sub1 y)))]
                 [else (append (list (LogicalLoc x (add1 y)))
                               (list (LogicalLoc x (sub1 y))))]))])))

;; return neighbors of same color
(: same-Ns : LogicalLoc Board -> (Listof LogicalLoc))
(define (same-Ns LL board)
  (local {(define st (bd-ref board LL))
          (define neighbs (neighbors LL board))
          (: lp : (Listof LogicalLoc) -> (Listof LogicalLoc))
          (define (lp LLs)
            (match LLs
              ['() '()]
              [(cons h t) (if (stone=? st (bd-ref board h)) (cons h (lp t)) (lp t))]))}
  (lp neighbs)))

;; return first list of LLs with those present in second list removed
(: drop-LLs : (Listof LogicalLoc) (Listof LogicalLoc) -> (Listof LogicalLoc))
(define (drop-LLs LLs1 LLs2)
  (match LLs1
    ['() '()]
    [(cons h1 t1)
     (local {(: lp : (Listof LogicalLoc) -> Boolean)
             (define (lp LLs)
               (match LLs
                 ['() #t]
                 [(cons h2 t2) (if (LL=? h1 h2) #f (lp t2))]))}
       (if (lp LLs2) (cons h1 (drop-LLs t1 LLs2)) (drop-LLs t1 LLs2)))]))

;; identify chain of connected stones
(: identify-chain : Board (Listof LogicalLoc) (Listof LogicalLoc) -> (Optional (Listof LogicalLoc)))
(define (identify-chain board to-explore marked)
  (match to-explore
    ['() (Some marked)]
    [(cons e t) (if (liberties? e board) 'None
                    (local {(define m (drop-LLs (same-Ns e board) marked))}
                    (identify-chain board (append m t) (cons e marked))))]))

(check-expect (identify-chain (vector
   '#(None None None None None None None None None None)
   '#(None None None None None None None None None None)
   '#(None None None None None None None None None None)
   '#(None None None None None None None None None None)
   '#(None None None None None None None None None None)
   '#(None None None None None None None None None None)
   (vector (Some 'black) 'None 'None 'None 'None 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'black) 'None 'None 'None 'None 'None 'None 'None 'None)
   (vector (Some 'black) 'None 'None 'None 'None 'None 'None 'None 'None 'None)
   '#(None None None None None None None None None None)) (list (LogicalLoc 7 0)) '())
              (Some (list (LogicalLoc 7 0))))

;; given placed-stone + board, return list of stones to be removed
(: toRemove : LogicalLoc Board -> (Listof (Optional (Listof LogicalLoc))))
(define (toRemove LL board)
  (local {(: lp : (Listof LogicalLoc) -> (Listof (Optional (Listof LogicalLoc))))
          (define (lp LLs)
            (match LLs
              ['() '()]
              [(cons h t) (cons (identify-chain board (list h) '()) (lp t))]))}
    (lp (cons LL (neighbors LL board)))))

(check-expect (toRemove (LogicalLoc 8 0) (vector
   '#(None None None None None None None None None None)
   '#(None None None None None None None None None None)
   '#(None None None None None None None None None None)
   '#(None None None None None None None None None None)
   '#(None None None None None None None None None None)
   '#(None None None None None None None None None None)
   (vector (Some 'black) 'None 'None 'None 'None 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'black) 'None 'None 'None 'None 'None 'None 'None 'None)
   (vector (Some 'black) 'None 'None 'None 'None 'None 'None 'None 'None 'None)
   '#(None None None None None None None None None None)))
              (list 'None 'None (Some (list (LogicalLoc 7 0))) 'None))

;; remove stones in given LogicalLocs from given board
(: remove-stones! : (Listof (Optional (Listof LogicalLoc))) Board -> Void)
(define (remove-stones! LLs board)
  (match LLs
    ['() (void)]
    [(cons h t)
     (match h
       ['None (remove-stones! t board)]
       [(Some '()) (remove-stones! t board)]
       [(Some (cons h1 t1)) (begin (bd-set! board h1 'None) (remove-stones! (cons (Some t1) t) board))])]))


;; update go by moving current board to history and making given board current
(: update-go : Go Board -> Go)
(define (update-go go bNew)
  (match go
    [(Go bOld next hist) (Go bNew (switch-turn next) (cons bOld hist))]))

;; apply move w game mechanics and everything
(: apply-move : Go LogicalLoc -> Go)
(define (apply-move go LL)
  (match go
    [(Go board next hist)
     (local {(define bcopy (board-copy board))}
       
     (begin
       (bd-set! bcopy LL (Some next))
       (remove-stones! (toRemove LL bcopy) bcopy)
       (update-go go bcopy)))]))
  

;; convert Vectorof Optional Stone -> list of PL
(: V->PLs : Integer BoardSpec (Vectorof (Optional Stone))
   (Optional Stone) (Listof PhysicalLoc) -> (Listof PhysicalLoc))
(define (V->PLs x BS vec stone PLs)
  (local
    {(define vlength (vector-length vec))
     (: loop : Integer -> (Listof PhysicalLoc))
     (define (loop y)
       (cond [(= y vlength) '()]
             [else (cond 
                     [(stone=? stone (vector-ref vec y))
                      (cons (logical->physical (LogicalLoc x y) vlength BS)
                                          (loop (add1 y)))]
                     [else (loop (add1 y))])]))}
    (append (loop 0) PLs)))

;; convert Board to listof PL of given color
(: Board->PLs : Board (Optional Stone) BoardSpec -> (Listof PhysicalLoc))
(define (Board->PLs board stone BS)
  (local
    {(define len (vector-length board))
     (: lp : Integer (Listof PhysicalLoc) -> (Listof PhysicalLoc))
     (define (lp i acc)
       (cond [(= i len) acc]
             [else (lp (add1 i)
                       (V->PLs i BS (vector-ref board i) stone acc))]))}
    (lp 0 '())))

;; draw grid of given dimension and cell size
(: grid : Integer Integer -> Image)
(define (grid n cell)
  (foldr above empty-image (make-list (sub1 n) 
     (foldr beside empty-image (make-list (sub1 n)
                                          (square cell 'outline 'black))))))

;; draw board given dimension and board-spec
(: draw-board : Integer BoardSpec String -> Image)
(define (draw-board n BS msg)
  (match BS
    [(BoardSpec color cell margin _)
     (above (overlay
      (grid n cell)
      (square (+ (* 2 margin) (* (sub1 n) cell)) 'solid color))
            (text msg 12 "black"))]))

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
                           (+ (* -1 x) r) (+ r (* -1 y))
                           (draw-stones t c r img))]))

;; draws world
(: draw : World -> Image)
(define (draw w)
  (match w
    [(World BS go msg)
     (match* (go BS)
       [((Go board next _) (BoardSpec col cell margin stone))
        (local {(define dim (vector-length board))}
          (beside (above (draw-stones (Board->PLs board (Some 'black) BS) (color 0 0 0) stone
                                      (draw-stones
                                       (Board->PLs board (Some 'white) BS) (color 255 255 255) stone
                                                   (draw-board dim BS msg)))
                         (label-bottom dim cell)) (label-side dim cell)))])]))

;; turns clicks into stone placement
(: react-to-click : World Integer Integer Mouse-Event -> World)
(define (react-to-click w x y m)
  (match m
    ["button-down"
     (match w
       [(World BS (Go board next hist) msg)
        (local {(define dim (vector-length board))
                (define a (physical->logical (PhysicalLoc x y) dim BS))}
          (match a
            [(Some LL)
             (if (legal-move? (Go board next hist) LL)
                 (World BS (apply-move (Go board next hist) LL)
                        (string-append
                         (symbol->string (switch-turn next))
                         "'s turn"))
                 (World BS (Go board next hist)
                        "that move would be against the RULES"))]
            ['None w]))])]
    [_ w]))

;; handles passing
(: react-to-p : World String -> World)
(define (react-to-p w k)
  (match k
    ["p" (match w
           [(World BS go msg)
            (match go
              [(Go a stone b) (World BS (Go a (switch-turn stone) b)
                                     (string-append
                                      (symbol->string stone)
                                      " has passed. "
                                      (symbol->string (switch-turn stone))
                                      "'s turn"))])])]
    [_ w]))

;; initiate empty Go
(: empty-Go : Integer -> Go)
(define (empty-Go n)
  (Go (empty-board n) 'black '()))
(: empty-board : Integer -> Board)
(define (empty-board dim)
  (build-vector dim
                (lambda ([i : Integer])
                  (make-vector dim (cast 'None (Optional Stone))))))

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
  
;; other test data
(define test-empty
  (empty-board 10))

(define test-board1
  (local {(define copy (board-copy test-empty))}
    (begin (bd-set! copy test-LL1 (Some 'black)) copy)))

(define test-board2
  (local {(define copy (board-copy test-board1))}
    (begin (bd-set! copy (LogicalLoc 0 0) (Some 'black)) copy)))

(: test-board3 : Board)
(define test-board3
  (vector
   '#(None None None None None None None None None None)
   '#(None None None None None None None None None None)
   '#(None None None None None None None None None None)
   '#(None None None None None None None None None None)
   (vector (Some 'white) (Some 'white) (Some 'white) 'None (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'black) (Some 'black) (Some 'white) (Some 'black) (Some 'white) (Some 'black) 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'white) 'None (Some 'black) 'None 'None 'None 'None 'None)
   '#(None None None None None None None None None None)
   '#(None None None None None None None None None None)
   '#(None None None None None None None None None None)))

(test)