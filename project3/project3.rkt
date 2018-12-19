#lang typed/racket

(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt") 
(require (only-in typed/racket/gui/base put-file get-file))

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
   [history : (Listof Board)]
   [last-turn-place : (Optional LogicalLoc)]
   [last-turn-opp-captures : (Listof LogicalLoc)]
   [last-turn-self-captures : (Listof LogicalLoc)]
   [consecutive-passes : Integer]))

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
   [status-message : String]
   [black-tenths : Integer]
   [white-tenths : Integer]
   [hover : (Optional LogicalLoc)]))

(define-struct Outcome
  ([black  : Integer]
   [white  : Integer]
   [winner : (U Stone 'draw)]))

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
        (define (rem a) (remainder (- a margin) cell))
        (: convert : Integer -> Integer)
        (define (convert a) (round (/ (- a margin) cell)))}
       (cond
         [(and (> (- x stone) 0) (< x (+ (* (sub1 n) cell) margin stone))
               (or (>= stone (rem x)) (>= stone (- cell (rem x))))
               (> (- y stone) 0) (< y (+ (* (sub1 n) cell) margin stone))
               (or (>= stone (rem y)) (>= stone (- cell (rem y)))))
                   (Some (LogicalLoc (convert x) (- (sub1 n) (convert y))))]
         [else 'None]))]))
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
  (local {(define dim (vector-length board))}
  (match LL
    [(LogicalLoc x y)
     (if (and (< x dim) (< y dim))
         (vector-ref (vector-ref board x) y)
         'None)])))

;; passes board from given Go to bd-ref
(: board-ref : Go LogicalLoc -> (Optional Stone))
(define (board-ref go LL)
  (match go 
    [(Go board _ _ _ _ _ _) (bd-ref board LL)]))

;; modify board to store given stone at given loc
(: bd-set! : Board LogicalLoc (Optional Stone) -> Void)
(define (bd-set! board LL stone)
  (match LL [(LogicalLoc x y)
             (begin (vector-set! (vector-ref board x) y stone))]))

;; modify given go to store given stone at given loc
(: board-set! : Go LogicalLoc (Optional Stone) -> Void)
(define (board-set! go LL stone)
  (match go
    [(Go board _ _ _ _ _ _)
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
       [(Go n stone h ll ocs scs cps) (begin (board-set! go LL (Some stone))
                              (Some (Go n (switch-turn stone) h ll ocs scs cps)))]
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
  (and (not (two-passes? go))
  (match (board-ref go LL)
    ['None
      (match go
        [(Go board next hist _ _ _ _)
         (check-hist hist (test-move board LL next))])]
      [_ #f])))

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
    [('None 'None) #t]
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
             [(= (sub1 dim) x) (stone-empty? (stone-at (LogicalLoc (sub1 x) y)))]
             [else (or (stone-empty? (stone-at (LogicalLoc (sub1 x) y)))
                       (stone-empty? (stone-at (LogicalLoc (add1 x) y))))])
           (cond
            [(= 0 y) (stone-empty? (stone-at (LogicalLoc x (add1 y))))]
            [(= (sub1 dim) y) (stone-empty? (stone-at (LogicalLoc x (sub1 y))))]
            [else (or (stone-empty? (stone-at (LogicalLoc x (add1 y))))
                      (stone-empty? (stone-at (LogicalLoc x (sub1 y)))))]))])))

;; return true if given loc is bordered only by empty spots or stones of given color
(: empties? : LogicalLoc Board Stone -> Boolean)
(define (empties? LL board st)
  (local {(define opp (switch-turn st))
          (define dim (vector-length board))
          (: stone-at : LogicalLoc -> (Optional Stone))
          (define (stone-at LL1) (bd-ref board LL1))}
    (match LL
      [(LogicalLoc x y)
       (not (or (cond
             [(= 0 x) (stone=? (Some opp) (stone-at (LogicalLoc (add1 x) y)))]
             [(= (sub1 dim) x)
              (stone=? (Some opp) (stone-at (LogicalLoc (sub1 x) y)))]
             [else (or (stone=? (Some opp) (stone-at (LogicalLoc (sub1 x) y)))
                       (stone=? (Some opp) (stone-at (LogicalLoc (add1 x) y))))])
           (cond
            [(= 0 y) (stone=? (Some opp) (stone-at (LogicalLoc x (add1 y))))]
            [(= (sub1 dim) y)
             (stone=? (Some opp) (stone-at (LogicalLoc x (sub1 y))))]
            [else (or (stone=? (Some opp) (stone-at (LogicalLoc x (add1 y))))
                      (stone=? (Some opp) (stone-at (LogicalLoc x (sub1 y)))))])))])))

(check-expect (empties? (LogicalLoc 4 0)
                        (vector '#(None None None None None None)
                                '#(None None None None None None)
                                '#(None None None None None None)
                                '#(None None None None None None)
                                (vector (Some 'white) (Some 'white) (Some 'white) (Some 'white) (Some 'white) (Some 'white))
                                '#(None None None None None None)) 'black)
              #f)

(check-expect (map (lambda ([x : LogicalLoc])
                      (empties? x
                                (vector '#(None None None None None None)
                                        '#(None None None None None None)
                                        '#(None None None None None None)
                                        (vector (Some 'white) (Some 'white) (Some 'white) (Some 'white) (Some 'white) 'None)
                                        (vector 'None (Some 'black) (Some 'black) (Some 'black) (Some 'black) (Some 'white))
                                        (vector (Some 'black) 'None 'None 'None 'None (Some 'black)))
                                'white))
                   (list (LogicalLoc 0 0) (LogicalLoc 0 1) (LogicalLoc 0 2)
                         (LogicalLoc 1 0) (LogicalLoc 2 0) (LogicalLoc 5 0)
                         (LogicalLoc 0 5) (LogicalLoc 5 5) (LogicalLoc 2 5)))
              '(#t #t #t #t #t #t #t #t #t))

(check-expect (map (lambda ([x : LogicalLoc])
                      (empties? x
                                (vector '#(None None None None None None)
                                        '#(None None None None None None)
                                        '#(None None None None None None)
                                        (vector (Some 'white) (Some 'white) (Some 'white) (Some 'white) (Some 'white) 'None)
                                        (vector 'None (Some 'black) (Some 'black) (Some 'black) (Some 'black) (Some 'white))
                                        (vector (Some 'black) 'None 'None 'None 'None (Some 'black)))
                                'black))
                   (list (LogicalLoc 0 0) (LogicalLoc 0 1) (LogicalLoc 0 2)
                         (LogicalLoc 1 0) (LogicalLoc 2 0) (LogicalLoc 5 0)
                         (LogicalLoc 0 5) (LogicalLoc 5 5) (LogicalLoc 2 5)))
'(#t #t #t #t #f #t #t #f #t))

;; return list of neighbors
(: neighbors : LogicalLoc Board -> (Listof LogicalLoc))
(define (neighbors LL board)
  (local {(define pos (bd-ref board LL))
          (define dim (vector-length board))}
    (match LL
      [(LogicalLoc x y)
       (append (cond
                 [(= 0 x) (list (LogicalLoc (add1 x) y))]
                 [(= (sub1 dim) x) (list (LogicalLoc (sub1 x) y))]
                 [else (append (list (LogicalLoc (add1 x) y))
                               (list (LogicalLoc (sub1 x) y)))])
               (cond
                 [(= 0 y) (list (LogicalLoc x (add1 y)))]
                 [(= (sub1 dim) y) (list (LogicalLoc x (sub1 y)))]
                 [else (append (list (LogicalLoc x (add1 y)))
                               (list (LogicalLoc x (sub1 y))))]))])))

;; return neighbors of given color
(: col-Ns : LogicalLoc Board (Optional Stone) -> (Listof LogicalLoc))
(define (col-Ns LL board st)
  (local {(define neighbs (neighbors LL board))
          (: lp : (Listof LogicalLoc) -> (Listof LogicalLoc))
          (define (lp LLs)
            (match LLs
              ['() '()]
              [(cons h t) (if (stone=? st (bd-ref board h))
                              (cons h (lp t)) (lp t))]))}
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
                    (local {(define m (drop-LLs (col-Ns e board (bd-ref board e)) marked))}
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

;; identify controlled area
(: identify-area : Board (Listof LogicalLoc) (Listof LogicalLoc) Stone -> (Listof LogicalLoc))
(define (identify-area board to-explore marked st)
  (match to-explore
    ['() marked]
    [(cons e t)
     (match (bd-ref board e)
       ['None (if (empties? e board st)
                    (identify-area board
                                   (append
                                    (drop-LLs (col-Ns e board 'None) marked) t)
                                   (cons e marked)
                                   st)
                    '())]
       [_ (error "id-area: given non-empty location")])]))

;; given placed-stone + board, return list of opponent captures
(: toRemove : LogicalLoc Board -> (Listof (Optional (Listof LogicalLoc))))
(define (toRemove LL board)
  (local {(define stone (bd-ref board LL))
          (: lp : (Listof LogicalLoc) -> (Listof (Optional (Listof LogicalLoc))))
          (define (lp LLs)
            (match LLs
              ['() '()]
              [(cons h t) (cons (identify-chain board (list h) '()) (lp t))]))}
    (match stone
      [(Some st)
       (lp (col-Ns LL board (Some (switch-turn st))))]
      ['None (error "toRemove: given empty location")])))

;; given placed-stone + board, return list of self captures
(: self-cap :  LogicalLoc Board -> (Listof LogicalLoc))
(define (self-cap LL bd)
  (local {(define self-caps
            (identify-chain bd (list LL) '()))}
    (match self-caps
      ['None '()]
      [(Some x) x])))

;; given list of optional list of LLs, return condensed list of LLs
(: condense-LLs : (Listof (Optional (Listof LogicalLoc))) -> (Listof LogicalLoc))
(define (condense-LLs LLs)
  (match LLs
    ['() '()]
    [(cons h t)
     (match h     
       [(Some (cons h1 t1)) (append (list h1)
                                    (condense-LLs (cons (Some t1) t)))]
       [_ (condense-LLs t)])]))

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
              (list (Some (list (LogicalLoc 7 0)))))

;; remove stones in given LogicalLocs from given board
(: remove-stones! : (Listof LogicalLoc) Board -> Void)
(define (remove-stones! LLs board)
  (match LLs
    ['() (void)]
    [(cons h t) (begin (bd-set! board h 'None) (remove-stones! t board))]))

;; update go by moving current board to history and making given board current
;; only called when legal move is made (not passes)
(: update-go : Go Board LogicalLoc (Listof LogicalLoc) (Listof LogicalLoc) -> Go)
(define (update-go go bNew LL OCs SCs)
  (match go
    [(Go bOld next hist _ _ _ _)
     (Go bNew (switch-turn next) (cons bOld hist) (Some LL) OCs SCs 0)]))                                       


;; return board-state that would result if given move were made 
(: test-move : Board LogicalLoc Stone -> Board)
(define (test-move bd LL next)
  (local {(define bcopy (board-copy bd))}
     (begin
       (bd-set! bcopy LL (Some next))
       (local {(define OCs (condense-LLs (toRemove LL bcopy)))}
       (remove-stones! OCs bcopy)
         (local {(define SCs (self-cap LL bcopy))}
           (remove-stones! SCs bcopy)))
       bcopy)))

;; apply move w game mechanics and everything
(: apply-move : Go LogicalLoc -> Go)
(define (apply-move go LL)
  (match go
    [(Go board next hist _ _ _ _)
     (local {(define bcopy (board-copy board))}
     (begin
       (bd-set! bcopy LL (Some next))
       (local {(define OCs (condense-LLs (toRemove LL bcopy)))}
       (remove-stones! OCs bcopy)
         (local {(define SCs (self-cap LL bcopy))}
           (remove-stones! SCs bcopy)
           (update-go go bcopy LL OCs SCs)))))]))
  

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

;; indicates if game is over
(: two-passes? : Go -> Boolean)
(define (two-passes? go)
  (match go
    [(Go _ _ _ _ _ _ cp) (= 2 cp)]))

;; counts number of given stones on given board
(: count-stones : Board Stone -> Integer)
(define (count-stones bd st)
  (local {(define len (vector-length bd))
          (: lp : Integer Integer Integer -> Integer)
          (define (lp i j acc)
            (cond
              [(= len i) acc]
              [(= len j) (lp (add1 i) 0 acc)]
              [else (if (stone=? (Some st)
                                 (bd-ref bd (LogicalLoc i j)))
                        (lp i (add1 j) (add1 acc))
                        (lp i (add1 j) acc))]))}
                         
            
    (lp 0 0 0)))

;; check if given LL is present in list
(: member-LL? : LogicalLoc (Listof LogicalLoc) -> Boolean)
(define (member-LL? LL LLs)
  (match LLs
    ['() #f]
    [(cons h t) (if (LL=? LL h) #t (member-LL? LL t))]))

;; generate list of all LLs to check to see who controls (for scoring)
(: all-LLs : Board -> (Listof LogicalLoc))
(define (all-LLs bd)
  (local
    {(define len (vector-length bd))
     (: lp : Integer Integer -> (Listof LogicalLoc))
     (define (lp i j)
       (cond
         [(= len i) '()]
         [(= j len) (lp (add1 i) 0)]
         [else (cons (LogicalLoc i j)
                     (lp i (add1 j)))]))}
    (lp 0 0)))

;; remove duplicates from list of LL
(: rem-d-LLs : (Listof LogicalLoc) -> (Listof LogicalLoc))
(define (rem-d-LLs LLs)
  (match LLs
    ['() '()]
    [(cons h t)
     (if (member-LL? h t)
         (rem-d-LLs t)
         (cons h (rem-d-LLs t)))]))

;; counts empty spots controlled by given player
(: empties : Board Stone -> (Listof LogicalLoc))
(define (empties bd st)
  (local
    {(define LLs (all-LLs bd))
     (: lp : (Listof LogicalLoc) (Listof LogicalLoc) -> (Listof LogicalLoc))
     (define (lp lls acc)
       (match lls
         ['() acc]
         [(cons h t)
          (if (member-LL? h acc)
              (lp t acc)
              (if (stone=? (bd-ref bd h) (Some st))
                  (lp t
                      (append (identify-area
                               bd (col-Ns h bd 'None)
                               '() st) acc))
                (lp t acc)))]))}
    (rem-d-LLs (lp LLs'()))))

;; return score of given player
(: score : Board Stone -> Integer)
(define (score bd st)
  (+ (length (empties bd st))
     (count-stones bd st)))

;; indicates outcome of game
(: outcome : Go -> Outcome)
(define (outcome go)
  (if (two-passes? go)
      (match go
        [(Go bd _ _ _ _ _ _)
         (local {(define blk (score bd 'black))
                 (define wt (score bd 'white))}
         (Outcome blk wt
                  (cond
                    [(> blk wt) 'black]
                    [(> wt blk) 'white]
                    [else 'draw])))])
      (error "outcome: can't score unfinished game")))

;; converts outcome to a message
(: outcome->string : Outcome -> String)
(define (outcome->string o)
  (match o
    [(Outcome bs ws winner)
     (cond 
       [(symbol=? winner 'draw) (string-append
               "Each player earned "
               (number->string bs)
               " points, drawing.")]
       [else (string-append
             (symbol->string winner)
             " won with "
             (number->string (max bs ws))
             " points. "
             (symbol->string (switch-turn winner))
             " earned "
             (number->string (min bs ws))
             " points.")])]))

;; draw grid of given dimension and cell size
(: grid : Integer Integer -> Image)
(define (grid n cell)
  (foldr above empty-image (make-list (sub1 n) 
     (foldr beside empty-image (make-list (sub1 n)
                                          (square cell 'outline 'black))))))

;; draw board given dimension and board-spec
(: draw-board : Integer BoardSpec -> Image)
(define (draw-board n BS)
  (match BS
    [(BoardSpec color cell margin _)
     (beside (above (overlay
                     (grid n cell)
                     (square (+ (* 2 margin) (* (sub1 n) cell)) 'solid color))
                    (label-bottom n cell))
             (label-side n cell))]))

;; create image of chars from size of board
(: label-bottom : Integer Integer -> Image)
(define (label-bottom dim cell)
  (match dim
    [0 empty-image]
    [x (beside (label-bottom (sub1 x) cell)
               (square (- cell 8) 0 'white)
               (text (string (int->char (sub1 x))) 12 "black"))]))

;; create image of ints from dimension
(: label-side : Integer Integer -> Image)
(define (label-side dim cell)
  (match dim
    [0 empty-image]
    [x (above (text (number->string x) 12 "black")
              (square (- cell 12) 0 'white)
               (label-side (sub1 x) cell))]))

;; convert num ms to image
(: timer : Integer String -> Image)
(define (timer ts stone)
  (local {(define ms (remainder (remainder ts 600) 10))
          (define secs (remainder (quotient ts 10) 60))}
    (above (text (string-append
                  (number->string (quotient ts 600))
                  ":"
                  (if (< secs 10) "0" "")
                  (number->string secs) "."
                  (number->string ms)) 15 "black")
           (text stone 15 "black"))))

;; create timer images
(: timers : Integer Integer -> Image)
(define (timers bts wts)
  (beside (timer bts "black")
          (square 30 'solid "white")
          (timer (+ bts wts) "total")
          (square 30 'solid "white")
          (timer wts "white")))

;; draw stones
(: draw-stones : (Listof PhysicalLoc) Color Integer Image -> Image)
(define (draw-stones PLs c r img)
  (match PLs
    ['() img]
    [(cons (PhysicalLoc x y) t)
     (overlay/align/offset "left" "top" (circle r 'solid c)
                           (+ (* -1 x) r) (+ r (* -1 y))
                           (draw-stones t c r img))]))
;; draw ghost stone
(: ghost : PhysicalLoc Stone Integer Image -> Image)
(define (ghost PL stone r img)
  (match PL
    [(PhysicalLoc x y)
     (overlay/align/offset "left" "top"
                           (circle r 120
                                   (if (stone=? (Some stone) (Some 'black))
                                       "black" "white"))
                           (+ (* -1 x) r) (+ r (* -1 y))
                           img)]))

;; draw last-placed
(: last-placed : (Optional LogicalLoc) Integer Integer BoardSpec Image -> Image)
(define (last-placed LL r dim BS img)
  (match LL
    ['None img]
    [(Some ll)
     (match (logical->physical ll dim BS)
       [(PhysicalLoc x y)
        (overlay/align/offset "left" "top"
                              (circle (/ r 2) 255 "green")
                              (+ (* -1 x) (* .5 r)) (+ (* .5 r) (* -1 y))
                              img)])]))

;; draw captured
(: captured : (Listof LogicalLoc) Integer Integer BoardSpec Color Image -> Image)
(define (captured LLs r dim BS col img)
  (match LLs
    ['() img]
    [(cons h t)
     (match (logical->physical h dim BS)
       [(PhysicalLoc x y)
        (overlay/align/offset "left" "top"
                              (circle (/ r 2) 255 col)
                              (+ (* -1 x) (* .5 r)) (+ (* .5 r) (* -1 y))
                              (captured t r dim BS col img))])]))

;; draws world
(: draw : World -> Image)
(define (draw w)
  (match w
    [(World BS go msg bt wt hov)
     (match* (go BS)
       [((Go board next _ last ocs scs _) (BoardSpec col cell margin stone))
        (local {(define dim (vector-length board))}
          (above (overlay/align "left" "top"
                                (match hov
                                  ['None (captured scs stone dim BS (color 255 0 0)
                                                   (captured ocs stone dim BS (color 0 0 255)
                                                             (last-placed last stone dim BS 
                                                                          (draw-stones (Board->PLs board (Some 'black) BS) (color 0 0 0) stone
                                                                                       (draw-stones
                                                                                        (Board->PLs board (Some 'white) BS) (color 255 255 255) stone
                                                                                        (draw-board dim BS))))))]
                                  [(Some LL) (ghost (logical->physical LL dim BS) next stone
                                                    (captured scs stone dim BS (color 255 0 0)
                                                              (captured ocs stone dim BS (color 0 0 255)
                                                                        (last-placed last stone dim BS
                                                                                     (draw-stones (Board->PLs board (Some 'black) BS) (color 0 0 0) stone
                                                                                                  (draw-stones
                                                                                                   (Board->PLs board (Some 'white) BS) (color 255 255 255) stone
                                                                                                   (draw-board dim BS)))))))])
                                (square 300 'solid "white"))
                 (text msg 12 "black")
                 (square 10 'outline "white")
                 (timers bt wt)))])]))


;; increment timers
(: react-to-tick (World -> World))
(define (react-to-tick w)
  (match w
    [(World BS go msg bt wt hov)
     (if (not (two-passes? go))
         (match go
           [(Go _ next _ _ _ _ _)
            (if (stone=? (Some next) (Some 'black))
                (World BS go msg (add1 bt) wt hov)
                (World BS go msg bt (add1 wt) hov))])
         w)]))

    
;; turns clicks into stone placement
(: react-to-click : World Integer Integer Mouse-Event -> World)
(define (react-to-click w x y m)
  (match m
    ["button-down"
     (match w
       [(World BS (Go board next hist ll ocs scs cps) msg bt wt _)
        (if (< cps 2)
            (local {(define dim (vector-length board))
                (define a (physical->logical (PhysicalLoc x y) dim BS))}
          (match a
            [(Some LL)
             (if (legal-move? (Go board next hist ll ocs scs cps) LL)
                 (World BS (apply-move (Go board next hist ll ocs scs cps) LL)
                        (string-append
                         (symbol->string next)
                         " placed at "
                         (logical->string LL)
                         ". "
                         (symbol->string (switch-turn next))
                         "'s turn")
                        bt wt 'None)
                 (World BS (Go board next hist ll ocs scs cps)
                        "RULES SAY: don't put it there idiot"
                        bt wt 'None))]
            ['None w]))
            w)])]
    ["move" 
     (match w
       [(World BS (Go board next hist ll ocs scs cps) msg bt wt _)
        (local {(define dim (vector-length board))
                (define a (physical->logical (PhysicalLoc x y) dim BS))}
          (match a
            [(Some LL)
             (if (legal-move? (Go board next hist ll ocs scs cps) LL)
                 (World BS (Go board next hist ll ocs scs cps) msg bt wt a)
                 (World BS (Go board next hist ll ocs scs cps) msg bt wt 'None))]
            [_ (World BS (Go board next hist ll ocs scs cps) msg bt wt 'None)]))])]
    [_ w]))

;; handles passing, saving, loading
(: react-to-p : World String -> World)
(define (react-to-p w k)
  (match k
    ["p" (match w
           [(World BS go msg bt wt hov)
            (if (not (two-passes? go))
            (match go
              [(Go a stone b _ _ _ 0) (World BS (Go a (switch-turn stone)
                                                      b 'None '() '() 1)
                                     (string-append
                                      (symbol->string stone)
                                      " has passed. "
                                      (symbol->string (switch-turn stone))
                                      "'s turn") bt wt hov)]
              [(Go a b c d e f _) (World BS (Go a b c d e f 2)
                                         (outcome->string (outcome (Go a b c d e f 2)))
                                         bt wt hov)])
            w)])]
    ["s" (begin (save-game! w) w)]
    ["l" (match w
           [(World BS (Go bdO _ _ _ _ _ _) _ _ _ _)
            (local
              {(define newW (load-game BS))}
              (match newW
                [(World _ (Go bd _ _ _ _ _ _) _ _ _ _)
                 (if (= (vector-length bd)
                        (vector-length bdO))
                     newW
                     (error "can't load game of different dimension in this window"))]))])]
    [_ w]))

;; initiate empty Go
(: empty-Go : Integer -> Go)
(define (empty-Go n)
  (Go (empty-board n) 'black '() 'None '() '() 0))
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
        (big-bang (World BS (empty-Go n) "black's turn" 0 0 'None) : World
          [to-draw draw]
          [on-mouse react-to-click]
          [on-key react-to-p]
          [on-tick react-to-tick 1/10])]
       [else (error "play: dimension too small")])]
    [else (error "play: invalid boardspec")]))

;; converts board to string
(: board->string : Board -> String)
(define (board->string bd)
  (local {(define len (vector-length bd))
          (: lp : Integer Integer -> String)
          (define (lp i j)
            (cond
              [(and (= (sub1 len) i) (= len j)) ""]
              [(= len j) (string-append "|" (lp (add1 i) 0))]
              [else (match (bd-ref bd (LogicalLoc i j))
                      ['None (string-append "_" (lp i (add1 j)))]
                      [(Some 'black) (string-append "*" (lp i (add1 j)))]
                      [(Some 'white) (string-append "o" (lp i (add1 j)))])]))}
    (lp 0 0)))

;; converst history to a string
(: hist->string : (Listof Board) -> String)
(define (hist->string hst)
  (match hst
    ['() ""]
    [(cons h '())  (board->string h)]
    [(cons h t) (string-append
                 (board->string h) "!"
                 (hist->string t))]))

;; converts go to string
(: go->string : Go -> String)
(define (go->string go)
  (match go
    [(Go board next hist _ _ _ cps)
     (string-append
      (match next
        ['black "*"]
        ['white "o"])
      "~" (board->string board)
      "~" (hist->string hist)
      "~" (number->string cps))]))
     

;; converts world to string (for file saving)
(: world->string : World -> String)
(define (world->string w)
  (match w
    [(World BS go _ bt wt _)
     (string-append
      (number->string bt) "@"
      (number->string wt) "@"
      (go->string go))]))

;; convert a text representation of an Integer to an Integer
(: string->integer : String -> Integer)
(define (string->integer s)
  (local
    {(define conv : (U Complex False) (string->number s))}
    (if (complex? conv) 
        (exact-round (real-part conv))
        (error "string->integer: invalid integer"))))

;; convert string to list of boards (history)
(: load-hist : String -> (Listof Board))
(define (load-hist s)
  (local {(: lp : (Listof String) -> (Listof Board))
          (define (lp Ss)
            (match Ss
              ['() '()]
              [(cons h t) (cons (load-board h)
                                (lp t))]))}
    (if (> (length (string-split s "!")) 0)
        (lp (string-split s "!"))
        (error "load-hist: history improperly formatted"))))

;; convert string to board
(: load-board : String -> Board)
(define (load-board s)
  (local {(define bList (string-split s "|"))
          (define len (length bList))
          (define bdd (empty-board len))
          (: lp : Integer Integer -> Board)
          (define (lp x y)
            (cond
              [(= x len) bdd]
              [(= y len) (lp (add1 x) 0)]
              [else (begin
                      (bd-set! bdd
                               (LogicalLoc x y)
                               (match (string-ref (list-ref bList x) y)
                                 [#\_ 'None]
                                 [#\* (Some 'black)]
                                 [#\o (Some 'white)]
                                 [_ (error "load-board: given file not properly formatted")]))
                      (lp x (add1 y)))]))}
    (if (= (length bList)
           (string-length (first bList)))
        (lp 0 0)
        (error "load-board: given board is not square"))))

;; converts string to Go
(: load-go : String -> Go)
(define (load-go s)
  (local {(define gList (string-split s "~"))}
    (Go
     (load-board (second gList))
     (match (first gList) ["*" 'black] ["o" 'white])
     (load-hist (third gList))
     'None '() '()
     (string->integer (last gList)))))

;; converts string to world
(: string->world : BoardSpec String -> World)
(define (string->world BS s)
  (local {(define wList (string-split s "@"))}
    (World
     BS
     (load-go (third wList))
     "game loaded: who knows who's turn it is?"
     (string->integer (first wList))
     (string->integer (second wList))
     'None)))

;; prompt the user for an output file location
;; then, save the game to that file
;; do nothing if the user cancels
(: save-game! : World -> Void)
(define (save-game! w)
  (local
    {(define path : (U Path False) (put-file))}
    (if (path? path)
        (local
          {(define op : Output-Port (open-output-file path))}
          (begin (write-string (world->string w) op)
                 (close-output-port op)))
        (void))))

;; ask the user to choose a file
;; then load an in-progress game from that file
;; use the provided BoardSpec to make a new World
;; raise an error if the user cancels or if something goes wrong
(: load-game : BoardSpec -> World)
(define (load-game bs)
  (local
    {(define path : (U Path False) (get-file))}
    (if (path? path)
        (local
          {(define ip : Input-Port (open-input-file path))
           (define w : World
             (string->world bs (port->string (open-input-file path))))}
          (begin (close-input-port ip) w))
        (error "load-game: user cancelled"))))

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