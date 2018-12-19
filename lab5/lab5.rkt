#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

(define-struct (Some a)
  ([value : a]))

(define-type (Optional a)
  (U 'None (Some a)))

(define-type Candidate
  Symbol)
  
(define-type Ballot
  (Listof Candidate))

(define-type Ballots
  (Listof Ballot))

(define-struct CandCount
  ([cand  : Candidate]
   [count : Integer]))

(define-type Tally
  (Listof CandCount))

;; test data
(define tBallots
  (list (list 'A 'C 'B) (list 'A 'B) (list 'B 'C) (list 'C 'B 'A) (list 'C 'B)))
(define tTL
  (list 'A 'A 'B 'C 'C))
(define tTally
  (list (CandCount 'A 2) (CandCount 'C 2) (CandCount 'B 1)))
(define tEmptyTally
  (list (CandCount 'A 0) (CandCount 'C 0) (CandCount 'B 0)))
(define tDropR1
   (list (list 'A 'C) (list 'A) (list 'C) (list 'C 'A) (list 'C)))

(define tBallots1
  (list (list 'A 'C 'B 'D) (list 'C 'A 'B) (list 'B 'D 'C) (list 'C 'B 'A) (list 'C 'B)))
(define tTL1
  (list 'A 'C 'B 'C 'C))
(define tTally1
  (list (CandCount 'A 1) (CandCount 'C 3) (CandCount 'B 1) (CandCount 'D 0)))
(define tEmptyTally1
  (list (CandCount 'A 0) (CandCount 'C 0) (CandCount 'B 0) (CandCount 'D 0)))
(define tDrop1R1
  (list (list 'A 'C 'B) (list 'C 'A 'B) (list 'B 'C) (list 'C 'B 'A) (list 'C 'B)))


;; if candidate is not present in tally add CandCount of 0
(: count0 : Tally Candidate -> Tally)
(define (count0 tally C)
  (match tally
    ['() (list (CandCount C 0))]
    [(cons (CandCount cd n) t)
     (if (symbol=? C cd)
         tally
         (cons (CandCount cd n) (count0 t C)))]))

;; iterate over all candidates to make sure they're in final tally
(: tally0 : Tally (Listof Candidate) -> Tally)
(define (tally0 tally candidates)
  (match candidates
    ['() tally]
    [(cons h t) (tally0 (count0 tally h) t)]))

;; go through candidates and create empty CandCount for each
(: tally-list : Tally (Listof Ballot) -> Tally)
(define (tally-list tally Bs)
  (match Bs
    ['() tally]
    [(cons h t) (tally-list (tally0 tally h) t)]))
(check-expect (tally-list '() tBallots) tEmptyTally)
(check-expect (tally-list '() tBallots1) tEmptyTally1)

;; take in a list of ballots and produce a list of first-choice candidates
(: first-list : Ballots -> (Listof Candidate))
(define (first-list Bs)
  (match Bs
    ['() '()]
    [(cons h t) (match h
       [(cons hb tb) (cons hb (first-list t))])]))
(check-expect (first-list tBallots) tTL)
(check-expect (first-list tBallots1) tTL1)

;; add 1 to given candidate's count in given tally
(: count1 : Tally Candidate -> Tally)
(define (count1 tally C)
  (match tally
    ['() '()]
    [(cons (CandCount cd n) t) (if (symbol=? cd C)
                                    (cons (CandCount C (add1 n)) t)
                                    (cons (CandCount cd n) (count1 t C)))]))
(check-expect (count1 tEmptyTally 'A) (list (CandCount 'A 1) (CandCount 'C 0) (CandCount 'B 0)))

;; loop over list of candidates, accumulating tally
(: count-cands : Tally (Listof Candidate) -> Tally)
(define (count-cands tally Cs)
  (match Cs
    ['() tally]
    [(cons c t) (count-cands (count1 tally c) t)]))
(check-expect (count-cands tEmptyTally tTL) tTally)
(check-expect (count-cands tEmptyTally1 tTL1) tTally1)

;; Count all the first-choice votes in the current round.
(: tally : Ballots -> Tally)
(define (tally Bs) 
    (count-cands (tally-list '() Bs) (first-list Bs)))
(check-expect (tally tBallots) tTally)
(check-expect (tally tBallots1) tTally1)

;; compare two CandCounts, return larger (if = return second one)
(: max-CC : CandCount CandCount -> CandCount)
(define (max-CC a b)
  (match* (a b)
    [((CandCount ca na) (CandCount cb nb))
     (if (> na nb) a b)]))

;; compare presumed min CandCounts with tally, return smallest CandCounts in tally
(: min-CC : (Listof CandCount) Tally -> (Listof CandCount))
(define (min-CC CCs tally)
  (match CCs
    ['() (error "min-CC: either you gave me an empty list of candidates
                 or you have no idea what you're doing")]
    [(cons (CandCount c1 n1) _)
     (match tally
       ['() CCs]
       [(cons (CandCount c2 n2) t2)
        (cond [(< n1 n2) (min-CC CCs t2)]
              [(> n1 n2) (min-CC (list (CandCount c2 n2)) t2)]
              [else (min-CC (cons (CandCount c2 n2) CCs) t2)])])]))

;; find if there is more than 1 top vote-getter,
;; given "winner" and tally
(: tie? : CandCount Tally -> Boolean)
(define (tie? CC tally)
  (match CC
    [(CandCount ca na)
     (match tally
       ['() #f]
       [(cons (CandCount cb nb) t)
        (if (and (= nb na) (not (symbol=? ca cb)))
            #t (tie? CC t))])]))
(check-expect (tie? (CandCount 'A 2) tTally) #t)
(check-expect (tie? (CandCount 'C 3) tTally1) #f)

;; Is there a winner in *this* round?
(: winner? : Ballots -> (Optional Candidate))
(define (winner? Bs)
  (local {(define winner (foldr max-CC (first (tally Bs)) (rest (tally Bs))))
          (define majority (quotient (length Bs) 2))}
    (if (tie? winner (tally Bs)) 'None
        (match winner
          [(CandCount c n)
           (if (> n majority) (Some c) 'None)]))))
(check-expect (winner? tBallots) 'None)
(check-expect (winner? tBallots1) (Some 'C))

;; identify losers' counts from tally
(: losersCount : Tally -> (Listof CandCount))
(define (losersCount tally)
  (match tally
    ['() '()]
    [(cons h t) (min-CC (list h) t)]))
(check-expect (losersCount tTally) (list (CandCount 'B 1)))
(check-expect (losersCount tTally1) (list (CandCount 'D 0)))

;; return list of loser candidates
(: losers : (Listof CandCount) -> (Listof Candidate))
(define (losers CCs)
  (match CCs
    ['() '()]
    [(cons (CandCount c n) t) (cons c (losers t))]))
(check-expect (losers (losersCount tTally)) '(B))
(check-expect (losers (losersCount tTally1)) '(D))

;; drop given candidate from given ballot
(: dropB : Ballot Candidate -> Ballot)
(define (dropB B C)
  (match B
    ['() '()]
    [(cons hb tb)
     (if (symbol=? C hb)
         (dropB tb C)
         (cons hb (dropB tb C)))]))

;; drop given candidates from given ballot
(: dropBCs : Ballot (Listof Candidate) -> Ballot)
(define (dropBCs B Cs)
  (match Cs
    ['() B]
    [(cons h t) (dropBCs (dropB B h) t)]))

;; drop trailing candidate(s)
(: drop : (Listof Ballot) -> (Listof Ballot))
(define (drop Bs)
  (local {(: lp : (Listof Ballot) (Listof Candidate) -> (Listof Ballot))
          (define (lp bals Cs)
            (match bals
              ['() '()]
              [(cons h t) (cons (dropBCs h Cs) (lp t Cs))]))}
    (lp Bs (losers (losersCount (tally Bs))))))
(check-expect (drop tBallots) tDropR1)
(check-expect (drop tBallots1) tDrop1R1)
            
;; Run rounds until Some winner is found. 
;; Eliminate the trailing candidate (or candidates if there is a tie) after each round.
;; Return 'None in case of no majority after any round, and no more rounds to run.
(: election : Ballots -> (Optional Candidate))
(define (election Bs)
  (if (tie? (foldr max-CC (first (tally Bs)) (rest (tally Bs))) (tally Bs))
      (election (drop Bs))
      (winner? Bs)))
(check-expect (election tBallots) (Some 'C))
(check-expect (election tBallots1) (Some 'C))

;; make string of candidates on given list
(: cand-string : (Listof Candidate) -> String)
(define (cand-string Cs)
  (match Cs
    ['() ""]
    [(cons h '()) (string-append (symbol->string h))]
    [(cons h t) (string-append (symbol->string h) ", " (cand-string t))]))

;; generate string detailing who was eliminated in round
(: elim-report : Ballots Integer -> String)
(define (elim-report Bs n)
  (local {(define loss (losers (losersCount (tally Bs))))}
    (if (= (length loss) 1)
        (string-append "Candidate: "
                       (cand-string loss)
                       " was eliminated in round "
                       (number->string n))
        (string-append "Candidates: "
                       (cand-string loss)
                       " were eliminated in round "
                       (number->string n)))))

;; convert election result to string
(: result-string : (Optional Candidate) -> String)
(define (result-string C)
  (match C
    ['None "There was no winner in this election after "]
    [(Some c) (string-append "Candidate "
                             (symbol->string c)
                             " won the election in ")]))

;; determine how many rounds needed to produce a winner
(: rounds-num : Ballots Integer -> Integer)
(define (rounds-num Bs n)
  (local {(define winner (winner? Bs))}
    (match winner
      ['None (rounds-num (drop Bs) (add1 n))]
      [_ n])))

;; produce an elimination report for each round that occurred
(: elim-reports : Ballots Integer -> String)
(define (elim-reports Bs n)
  (match n
    [0 ""]
    [x (string-append (elim-reports (drop Bs) (sub1 n))
                      (elim-report Bs n) ". ")])) 
           
;; describe the election
(: election-report : Ballots -> String)
(define (election-report Bs)
  (string-append (result-string (election Bs))
                 (number->string (rounds-num Bs 1))
                 " round"
                 (if (> (rounds-num Bs 1) 1) "s. " ". ")
                 (elim-reports Bs (rounds-num Bs 1))))

(test)