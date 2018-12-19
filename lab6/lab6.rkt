#lang typed/racket

(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

(define-type (Order a)
  (a a -> Boolean))

(define-struct (Some a)
  ([value : a]))

(define-type (Optional a)
  (U 'None (Some a)))

(define-struct (Pair a b)
  ([item1 : a]
   [item2 : b]))

(define-struct (Nd a b)
  ([key   : a]
   [value : b]
   [lsub : (BST a b)]
   [rsub : (BST a b)]))

(define-type (BST a b)
  (U 'E (Nd a b)))

;; The type of a BST map that maps alphas to betas.
(define-struct (BSTMap a b)
  ([ord  : (Order a)]
   [data : (BST a b)]))

;; If the given key is in the map and associated with value v, return Some v.
;; If the key is absent from the map, return 'None.
(: lookup : All (a b) a (BSTMap a b) -> (Optional b))
(define (lookup search map)
  (match map
    [(BSTMap ord data)
     (match* (data search)
       [('E _) 'None]
       [((Nd k v ls rs) k) (Some v)]
       [((Nd k v ls rs) _)(if (ord search k) (lookup search (BSTMap ord ls))
                 (lookup search (BSTMap ord rs)))])]))
(check-expect (lookup 4 tbstm1) (Some "four"))
(check-expect (lookup 8 tbstm1) (Some "eight"))
(check-expect (lookup 13 tbstm1) 'None)

;; Insert the given key/value pair into the map.
;; If the given key is already present in the tree, replace its value with the new one.
(: insert : All (a b) a b (BSTMap a b) -> (BSTMap a b))
(define (insert key val map)
  (match map
    [(BSTMap ord dat)
     (local {(: lp : (BST a b) -> (BST a b))
          (define (lp t)
            (match* (t key)
              [('E k) (Nd key val 'E 'E)]
              [((Nd k v ls rs) k) (Nd key val ls rs)]
              [((Nd k v ls rs) _) (if (ord key k) (Nd k v (lp ls) rs)
                        (Nd k v ls (lp rs)))]))}
     (BSTMap ord (lp dat)))]))
(check-expect (BSTMap-data (insert 11 "eleven" tbstm1)) (BSTMap-data tbstm2))
(check-expect (BSTMap-data (insert 13 "thirteen" tbstm2)) (BSTMap-data tbstm3))
(check-expect (BSTMap-data (insert 13 "thirdeen" tbstm3)) (BSTMap-data tbstm4))

;; Return all the keys from the map in left-to-right order, i.e., an inorder traversal.
(: keys-inorder : All (a b) (BSTMap a b) -> (Listof a))
(define (keys-inorder bstm)
  (match bstm
    [(BSTMap ord dat)
     (local {(: lp : (BST a b) -> (Listof a))
             (define (lp t)
               (match t
                 ['E '()]
                 [(Nd k v ls rs) (append (lp ls)
                                         (list k)
                                         (lp rs))]))}
       (lp dat))]))
(check-expect (keys-inorder tbstm3) '(1 2 3 4 5 6 7 8 9 10 11 12 13))

;; Return all the key/value pairs from the map in left-to-right order
(: pairs-inorder : All (a b) (BSTMap a b) -> (Listof (Pair a b)))
(define (pairs-inorder bstm)
  (local {(define keys (keys-inorder bstm))
          (: lp : (Listof a) -> (Listof (Pair a b)))
          (define (lp ks)
            (match ks
              ['() '()]
              [(cons h t) (match (lookup h bstm)
                            [(Some v) (cons (Pair h v) (lp t))]
                            ['None (error "pairs-inorder: somehow got a key not in tree")])]))}
    (lp keys)))
(check-expect (pairs-inorder tbstm3) (list (Pair 1 "one") (Pair 2 "two") (Pair 3 "three")
                                           (Pair 4 "four") (Pair 5 "five") (Pair 6 "six")
                                           (Pair 7 "seven") (Pair 8 "eight") (Pair 9 "nine")
                                           (Pair 10 "ten") (Pair 11 "eleven") (Pair 12 "twelve")
                                           (Pair 13 "thirteen")))

;; Return the leftmost key/value pair in the tree.
(: leftmost : All (a b) (BST a b) -> (Pair a b))
(define (leftmost t)
  (match t
    ['E (error "leftmost: cannot find leftmost of empty tree")]
    [(Nd k v 'E rs) (Pair k v)]
    [(Nd k v ls rs) (leftmost ls)]))
(check-expect (leftmost (BSTMap-data tbstm1)) (Pair 1 "one"))

;; Remove the leftmost key/value pair in the tree.
(: remove-leftmost : All (a b) (BST a b) -> (BST a b))
(define (remove-leftmost t)
  (match t
    ['E 'E]
    [(Nd k v (Nd kl vl 'E rsl) rs) (Nd k v rsl rs)]
    [(Nd k v ls rs) (Nd k v (remove-leftmost ls) rs)]))
(check-expect (remove-leftmost (BSTMap-data tbstm3)) (BSTMap-data tbstm5))

;; Remove the key/value pair at the root of the tree.
(: remove-root : All (a b) (BST a b) -> (BST a b))
(define (remove-root t)
  (match t
    ['E 'E]
    [(Nd _ _ ls 'E) ls]
    [(Nd _ _ ls rs) (Nd (Pair-item1 (leftmost rs))
                        (Pair-item2 (leftmost rs))
                        ls (remove-leftmost rs))]))
(check-expect (remove-root (BSTMap-data tbstm1)) (BSTMap-data tbstm6))

;; Remove the given key, and its value, from the map.
;; If that key is absent from the map, return the map as is.
(: remove : All (a b) a (BSTMap a b) -> (BSTMap a b))
(define (remove key bstm)
  (match bstm
    [(BSTMap ord dat)
     (local
       {(: lp : (BST a b) -> (BST a b))
        (define (lp t)
          (match* (t key)
            [('E _) t]
            [((Nd k v ls rs) k) (remove-root (Nd k v ls rs))]
            [((Nd k v ls rs) _) (if (ord key k)
                                    (Nd k v (lp ls) rs)
                                    (Nd k v ls (lp rs)))]))}
       (BSTMap ord (lp dat)))]))
(check-expect (BSTMap-data (remove 3 tbstm1)) (BSTMap-data tbstm7))

;; test data
(: tbstm1 : (BSTMap Integer String))
(define tbstm1
  (BSTMap < (Nd 6 "six"
                      (Nd 3 "three"
                               (Nd 1 "one" 'E
                                    (Nd 2 "two" 'E 'E))
                               (Nd 5 "five"
                                        (Nd 4 "four" 'E 'E)
                                       'E))
                      (Nd 9 "nine"
                               (Nd 7 "seven" 'E
                                        (Nd 8 "eight" 'E 'E))
                               (Nd 10 "ten" 'E
                                        (Nd 12 "twelve" 'E 'E))))))

(: tbstm2 : (BSTMap Integer String))
(define tbstm2
  (BSTMap < (Nd 6 "six"
                      (Nd 3 "three"
                               (Nd 1 "one" 'E
                                    (Nd 2 "two" 'E 'E))
                               (Nd 5 "five"
                                        (Nd 4 "four" 'E 'E)
                                       'E))
                      (Nd 9 "nine"
                               (Nd 7 "seven" 'E
                                        (Nd 8 "eight" 'E 'E))
                               (Nd 10 "ten" 'E
                                        (Nd 12 "twelve" (Nd 11 "eleven" 'E 'E) 'E))))))
(: tbstm3 : (BSTMap Integer String))
(define tbstm3
  (BSTMap < (Nd 6 "six"
                      (Nd 3 "three"
                               (Nd 1 "one" 'E
                                    (Nd 2 "two" 'E 'E))
                               (Nd 5 "five"
                                        (Nd 4 "four" 'E 'E)
                                       'E))
                      (Nd 9 "nine"
                               (Nd 7 "seven" 'E
                                        (Nd 8 "eight" 'E 'E))
                               (Nd 10 "ten" 'E
                                        (Nd 12 "twelve"
                                            (Nd 11 "eleven" 'E 'E)
                                            (Nd 13 "thirteen" 'E 'E)))))))

(: tbstm4 : (BSTMap Integer String))
(define tbstm4
  (BSTMap < (Nd 6 "six"
                      (Nd 3 "three"
                               (Nd 1 "one" 'E
                                    (Nd 2 "two" 'E 'E))
                               (Nd 5 "five"
                                        (Nd 4 "four" 'E 'E)
                                       'E))
                      (Nd 9 "nine"
                               (Nd 7 "seven" 'E
                                        (Nd 8 "eight" 'E 'E))
                               (Nd 10 "ten" 'E
                                        (Nd 12 "twelve"
                                            (Nd 11 "eleven" 'E 'E)
                                            (Nd 13 "thirdeen" 'E 'E)))))))

(: tbstm5 : (BSTMap Integer String))
(define tbstm5
  (BSTMap < (Nd 6 "six"
                      (Nd 3 "three"
                               (Nd 2 "two" 'E 'E)
                               (Nd 5 "five"
                                        (Nd 4 "four" 'E 'E)
                                       'E))
                      (Nd 9 "nine"
                               (Nd 7 "seven" 'E
                                        (Nd 8 "eight" 'E 'E))
                               (Nd 10 "ten" 'E
                                        (Nd 12 "twelve"
                                            (Nd 11 "eleven" 'E 'E)
                                            (Nd 13 "thirteen" 'E 'E)))))))

(: tbstm6 : (BSTMap Integer String))
(define tbstm6
  (BSTMap < (Nd 7 "seven"
                      (Nd 3 "three"
                               (Nd 1 "one" 'E
                                    (Nd 2 "two" 'E 'E))
                               (Nd 5 "five"
                                        (Nd 4 "four" 'E 'E)
                                       'E))
                      (Nd 9 "nine"
                                        (Nd 8 "eight" 'E 'E)
                               (Nd 10 "ten" 'E
                                        (Nd 12 "twelve" 'E 'E))))))


(: tbstm7 : (BSTMap Integer String))
(define tbstm7
  (BSTMap < (Nd 6 "six"
                      (Nd 4 "four"
                               (Nd 1 "one" 'E
                                    (Nd 2 "two" 'E 'E))
                               (Nd 5 "five" 'E 'E))
                      (Nd 9 "nine"
                               (Nd 7 "seven" 'E
                                        (Nd 8 "eight" 'E 'E))
                               (Nd 10 "ten" 'E
                                        (Nd 12 "twelve" 'E 'E))))))
(test)