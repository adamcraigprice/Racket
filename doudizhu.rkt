;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname doudizhu) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))

;; ***************************************************
;; Hitesh Gupta (21075474)
;; CS 135 Fall 2023
;; Assignment A05, Task 3
;; ***************************************************

;; DATA DEFINITIONS:

;; A Card is one of:
;; Nat in range [2, 10]
;; One of the following Sym: 'Jack 'Queen 'King 'Ace 'Black 'Red

;; A Hand is a (listof Card)

;;
;; Problem A
;;

;; (solos hand) makes a list of lists of all possible playable solo cards

;; Examples:
(check-expect
(solos (list 3 3 3 3 4 4 5 5 6 7 7 7 9 'Jack 'Jack 'Queen 'King 'Ace 2 2))
(list (list 3) (list 4) (list 5) (list 6) (list 7)
(list 9) (list 'Jack) (list 'Queen) (list 'King)
(list 'Ace) (list 2)))
(check-expect
(solos (list 4 5 7 'Queen 'Ace))
(list (list 4) (list 5) (list 7) (list 'Queen) (list 'Ace)))

;; solos: Hand -> (listof Hand)
(define (solos hand) (remove-duplicates (solo-lists hand)))

;; Tests:
(check-expect
(solos (list 3 4 5 6 7 8 9 10 2))
(list (list 3) (list 4) (list 5) (list 6) (list 7) (list 8)
(list 9) (list 10) (list 2)))
(check-expect
(solos (list 5 5 'Jack 'Jack 'Queen 'King 'Ace 'Ace 'Ace))
(list (list 5) (list 'Jack) (list 'Queen) (list 'King) (list 'Ace)))

;; HELPER FUNCTIONS

(define (solo-lists hand)
  (cond
    [(empty? hand) empty]
    [else (cons (list (first hand)) (solo-lists (rest hand)))]))

(check-expect
(solo-lists (list 4 5 5 'Queen 'Ace))
(list (list 4) (list 5) (list 5) (list 'Queen) (list 'Ace)))

(define (remove-duplicates list)
  (cond
    [(empty? list) empty]
    [(member? (first list) (rest list)) (remove-duplicates (rest list))]
    [else (cons (first list) (remove-duplicates (rest list)))]))

(check-expect (remove-duplicates (cons 'symbol (cons 3 (cons 8 (cons 2
(cons 4 (cons 2 (cons 7 (cons 'symbol (cons 5 empty))))))))))
(cons 3 (cons 8 (cons 4 (cons 2 (cons 7 (cons 'symbol (cons 5 empty))))))))
(check-expect (remove-duplicates (cons 8 (cons 2 (cons 3 empty))))
(cons 8 (cons 2 (cons 3 empty))))
(check-expect (remove-duplicates (cons "blue" (cons "blue" (cons "blue" empty))))
(cons "blue" empty))

;;
;; Problem B
;;

;; (pairs hand) makes a list of lists of all possible playable pairs

;; Examples:
(check-expect
(pairs (list 3 3 3 'Queen 'King 'King 'Red 'Red 'Red 'Red))
(list (list 3 3) (list 'King 'King) (list 'Red 'Red)))
(check-expect
(pairs (list 5 5 'Jack 'Jack 'Queen 'King 'Ace 'Ace 'Ace))
(list (list 5 5) (list 'Jack 'Jack) (list 'Ace 'Ace)))

;; pairs: Hand -> (lstof Hand)
(define (pairs hand) (pair-lists (find-kind 2 hand)))

;; Tests:
(check-expect
(pairs (list 3 3 3 3 4 5 6 7 7 7
'Jack 'Jack 'Queen 'King 'Ace 2 2))
(list (list 3 3) (list 7 7) (list 'Jack 'Jack) (list 2 2)))
(check-expect
(pairs (list 4 5 7 'Queen 'Ace))
(list))

;; HELPER FUNCTIONS

(define (pair-lists cards)
  (cond
    [(empty? cards) empty]
    [else (cons (list (first cards) (first cards)) (pair-lists (rest cards)))]))

(check-expect (pair-lists (list 4 6 'Black)) (list (list 4 4) (list 6 6) (list 'Black 'Black)))

(define (find-kind num-occurence sorted)
  (cond
    [(= num-occurence 1) (remove-duplicates sorted)]
    [else (find-kind (- num-occurence 1) (remove-one-of-each sorted))]))

(check-expect
 (find-kind 2 (list 3 5 7 7 8 8 8 8 8 'Ace 'Red)) (list 7 8))
(check-expect
 (find-kind 5 (list 3 3 3 5 5 6 6 7 'Queen 'Black)) (list))
(check-expect
 (find-kind 2 (list 4 4)) (list 4))

(define (remove-one-of-each sorted)
  (cond
    [(empty? sorted) empty]
    [(member? (first sorted) (rest sorted))
     (cons (first sorted) (remove-one-of-each (rest sorted)))]
    [else (remove-one-of-each (rest sorted))]))

(check-expect (remove-one-of-each
               (list 3 3 3 3 4 5 6 7 7 7 'Jack 'Jack 'Queen 'King 'Ace 2 2)) (list 3 3 3 7 7 'Jack 2))
(check-expect (remove-one-of-each
               (list 3 'Jack 'Queen 'Ace 2)) (list))

;;
;; Problem C
;;

;; (trios hand) makes a list of lists of all possible playable trios

;; Examples:
(check-expect
(trios (list 3 3 3 3 3 4 4 4 'Queen 'King 'King 'King 'Red 'Red 'Red 'Red))
(list (list 3 3 3) (list 4 4 4) (list 'King 'King 'King) (list 'Red 'Red 'Red)))
(check-expect
(trios (list 5 5 'Jack 'Jack 'Queen 'King 'Ace 'Ace 'Ace))
(list (list 'Ace 'Ace 'Ace)))

;; trios: Hand -> (lstof Hand)
(define (trios hand) (trio-lists (find-kind 3 hand)))

;; Tests:
(check-expect
(trios (list 3 3 3 3 4 5 6 7 7 7
'Jack 'Jack 'Queen 'King 'Ace 2 2))
(list (list 3 3 3) (list 7 7 7)))
(check-expect
(trios (list 4 5 7 'Queen 'Ace))
(list))

;; HELPER FUNCTION

(define (trio-lists cards)
  (cond
    [(empty? cards) empty]
    [else (cons (list (first cards) (first cards) (first cards)) (trio-lists (rest cards)))]))

(check-expect (trio-lists (list 4 'Ace)) (list (list 4 4 4) (list 'Ace 'Ace 'Ace)))

;;
;; Problem D
;;

;; (sort-hands hand-list) takes a list of hands and puts them in order based on ranking

;; Examples:
(check-expect
(sort-hands (list (list 'Jack 'Queen 'King) (list 3 3 3) (list 4 4)
(list 'Black 'Red) (list 4 4) (list 5 5 5) (list 3 4 5) (list 3 3 3)))
(list (list 4 4) (list 3 3 3) (list 3 4 5) (list 5 5 5)
(list 'Jack 'Queen 'King) (list 'Black 'Red)))
(check-expect
(sort-hands (list (list 3 4 5) (list 7 7 7 7) (list 2) (list 9) (list 'Queen 'King)
(list 'Black 'Red) (list 'Jack 'Queen 'King 'Ace)))
(list (list 9) (list 2) (list 'Queen 'King) (list 3 4 5) (list 'Jack 'Queen 'King 'Ace)
(list 7 7 7 7) (list 'Black 'Red)))

;; sort-hands: (lstof Hand) -> (lstof Hand)
(define (sort-hands hand-list) (remove-duplicates (sort-hands-bulk hand-list)))

;; Tests:
(check-expect
(sort-hands (list (list 3 4 2) (list 6 6 6 6) (list 7 8 9 10) (list 2 2 2 2)
(list 5 5 5 6) (list 'Jack 'Queen 'King 'Ace)))
(list (list 3 4 2) (list 5 5 5 6) (list 7 8 9 10) (list 'Jack 'Queen 'King 'Ace)
(list 6 6 6 6) (list 2 2 2 2)))
(check-expect
(sort-hands (list (list 'Black 'Red) (list 2 2 2 2) (list 3 4 5 6 7 8 9 10)
(list 5) (list 2) (list 3 3)))
(list (list 5) (list 2) (list 3 3) (list 3 4 5 6 7 8 9 10)
(list 2 2 2 2) (list 'Black 'Red)))

;; HELPER FUNCTIONS

(define (sort-hands-bulk hand-list)
  (cond [(empty? hand-list) empty]
        [else (insert (first hand-list)
                      (sort-hands (rest hand-list)))]))

(define (insert hand sorted)
  (cond [(empty? sorted) (list hand)]
        [(hand<? hand (first sorted)) (cons hand sorted)]
        [else (cons (first sorted) (insert hand (rest sorted)))]))

(check-expect (insert (list 5 5) (list (list 4 4) (list 3 3 3) (list 3 4 5) (list 5 5 5)
(list 'Jack 'Queen 'King) (list 'Black 'Red)))
(list (list 4 4) (list 5 5) (list 3 3 3) (list 3 4 5) (list 5 5 5)
(list 'Jack 'Queen 'King) (list 'Black 'Red)))
(check-expect (insert (list 'Black 'Red) (list (list 4 4) (list 3 3 3) (list 3 4 5) (list 5 5 5)
(list 'Jack 'Queen 'King)))
(list (list 4 4) (list 3 3 3) (list 3 4 5) (list 5 5 5)
(list 'Jack 'Queen 'King) (list 'Black 'Red)))

(define (hand<? hand1 hand2)
  (cond
    [(rocket? hand2) true]
    [(bomb? hand2) (cond
                     [(is-higher? (first hand2) (first hand1)) true]
                     [else false])]
    [(or (rocket? hand1) (bomb? hand1)) false]
    [(< (length hand1) (length hand2)) true]
    [(> (length hand1) (length hand2)) false]
    [else (same-size-higher? hand1 hand2)]))

(check-expect (hand<? (list 3 3 3 3) (list 5 6 7)) false)
(check-expect (hand<? (list 2 2 2 2) (list 8 8 8 8)) false)
(check-expect (hand<? (list 2 2 2 2) (list 'Black 'Red)) true)
(check-expect (hand<? (list 7) (list 1 1)) true)
(check-expect (hand<? (list 10) (list 2)) true)
(check-expect (hand<? (list 'Jack 'Queen 'King) (list 'Black 'Red)) true)
(check-expect (hand<? (list 3 3 3) (list 'Black 'Red)) true)


(define (same-size-higher? hand1 hand2)
  (cond
    [(empty? hand1) false]
    [(is-higher? (first hand2) (first hand1)) true]
    [(and (not (card=? (first hand1) (first hand2)))
          (not (is-higher? (first hand2) (first hand1)))) false]
    [else (same-size-higher? (rest hand1) (rest hand2))]))

(check-expect (same-size-higher? (list 3 3 4 5) (list 2 7 8 3)) true)
(check-expect (same-size-higher? (list 3 3 4 5) (list 3 3 4 6)) true)
(check-expect (same-size-higher? (list 3 3 4 6) (list 3 3 4 5)) false)
(check-expect (same-size-higher? (list 3 3 4 2) (list 3 3 4 5)) false)

(define (card=? card1 card2)
  (cond
    [(and (symbol? card1) (symbol? card2) (symbol=? card1 card2)) true]
    [(and (integer? card1) (integer? card2) (= card1 card2)) true]
    [else false]))

(check-expect (card=? 'King 'King) true)
(check-expect (card=? 'King "King") false)
(check-expect (card=? 'King 'Kings) false)

(define (is-higher? card1 card2)
  (cond
    [(card=? card1 card2) false]
    [(and (integer? card1) (integer? card2)) (cond
                  [(and (> card2 card1) (not (= 2 card1))) false]
                  [(= 2 card2) false]
                  [else true])]
    [(and (symbol? card1) (integer? card2)) (cond
                  [(and (= 2 card2) (not (or (symbol=? card1 'Red) (symbol=? card1 'Black)))) false]
                  [else true])]
    [(and (integer? card1) (symbol? card2)) (cond
                  [(and (= 2 card1) (not (or (symbol=? card2 'Red) (symbol=? card2 'Black)))) true]
                  [else false])]
    [(symbol=? card1 'Red) true]
    [(and (symbol=? card1 'Black) (not (symbol=? card2 'Red))) true]
    [(and (symbol=? card1 'Ace) (not (or (symbol=? card2 'Black) (symbol=? card2 'Red)))) true]
    [(and (symbol=? card1 'King) (or (symbol=? card2 'Queen) (symbol=? card2 'Jack))) true]
    [(and (symbol=? card1 'Queen) (symbol=? card2 'Jack)) true]
    [else false]))

(check-expect (is-higher? 2 3) true)
(check-expect (is-higher? 3 7) false)
(check-expect (is-higher? 'King 'Jack) true)
(check-expect (is-higher? 'Queen 'Ace) false)
                             
(define (rocket? hand)
  (cond
  [(empty? hand) false]
  [(and
    (and (symbol? (first hand)) (symbol=? (first hand) 'Black))
    (and (symbol? (second hand)) (symbol=? (second hand) 'Red))
    (empty? (rest (rest hand)))) true]
  [else false]))

(check-expect (rocket? (list 'Black 'Red)) true)
(check-expect (rocket? (list 'Black 'Black)) false)
(check-expect (rocket? (list 'Red 'Red)) false)
(check-expect (rocket? (list 'Black 2)) false)
(check-expect (rocket? (list)) false)
(check-expect (rocket? (list 4 5 6 2)) false)

(define (bomb? hand)
  (cond
    [(and (= 1 (length (find-kind 1 hand))) (= 4 (length hand))) true]
    [else false]))

(check-expect (bomb? (list 5 5 5 5)) true)
(check-expect (bomb? (list 5 5 5 3)) false)
(check-expect (bomb? (list 5 5 5)) false)
(check-expect (bomb? (list 5 5 5 5 3)) false)
(check-expect (bomb? (list 'Black 'Black 'Black 'Black)) true)

;;
;; Problem E
;;

;; (straights hand) produces a list of all the playable straights in a hand

;; Examples:
(check-expect
(straights (list 3 3 3 3 4 5 6 7 8 9 'Jack 'Jack 'Queen 'King 'Ace 2 2))
(list (list 3 4 5 6 7) (list 4 5 6 7 8) (list 5 6 7 8 9)
(list 3 4 5 6 7 8) (list 4 5 6 7 8 9) (list 3 4 5 6 7 8 9)))

(check-expect
(straights (list 3 4 5 6 7 9 10 'Jack 'Queen 'King 'King 'Ace 2 'Black 'Red 'Red))
(list (list 3 4 5 6 7) (list 9 10 'Jack 'Queen 'King) (list 10 'Jack 'Queen 'King 'Ace)
      (list 9 10 'Jack 'Queen 'King 'Ace)))

;; straights: Hand -> (lstof Hand)
(define (straights hand)
  (cond
    [(> 5 (length hand)) empty]
  [else (sort-hands (remove-duplicates (bulk-straights
                (big-append empty
                     (subseqfull (remove-duplicates hand) (length (remove-duplicates hand)))))))]))

;; Tests:
(check-expect
(straights (list 3 4 7 8 9 10 'Jack))
(list (list 7 8 9 10 'Jack)))

(check-expect
(straights (list 3 4 6 8 9 10 'Queen 'Queen 'Queen 'King 'Ace 2 2 2 'Red))
(list))

;; HELPER FUNCTIONS

(define (bulk-straights nested)
  (cond
    [(empty? nested) empty]
    [(straight? (first nested) (first nested)) (cons (first nested) (bulk-straights (rest nested)))]
    [else (bulk-straights (rest nested))]))

(check-expect (bulk-straights (list (list 4 5 6) (list 7 8 9 10 'Jack) (list 4 5 6 7 8 9 10) (list 1))
)(list (list 7 8 9 10 'Jack) (list 4 5 6 7 8 9 10)))
(check-expect (bulk-straights (list (list 3 4 5 7 9) (list 10 'Jack 'Queen 'King 'Ace)))
              (list (list 10 'Jack 'Queen 'King 'Ace)))

(define (straight? pot-straight pot-straight-rec)
  (cond
    [(> 5 (length pot-straight)) false]
    [(empty? (rest pot-straight-rec)) true]
    [(or (> (card-values (second pot-straight-rec)) 14)
         (> (card-values (first pot-straight-rec)) 14)) false]
    [(= 1 (- (card-values (second pot-straight-rec)) (card-values (first pot-straight-rec))))
     (and true (straight? pot-straight (rest pot-straight-rec)))]
    [else false]))

(check-expect (straight? (list 3 4 5 6 7) (list 3 4 5 6 7)) true)
(check-expect (straight? (list 3 4 5 6) (list 3 4 5 6)) false)
(check-expect (straight? (list 3 4 5 6 7 9) (list 3 4 5 6 7 9)) false)
(check-expect (straight? (list 3 4 5 6 7 8) (list 3 4 5 6 7 8)) true)
(check-expect (straight? (list 3 4 5 6 7 8) (list 3 4 5 6 7 8)) true)
(check-expect (straight? (list 9 10 'Jack 'Queen 'King 'Ace)
                         (list 9 10 'Jack 'Queen 'King 'Ace)) true)
(check-expect (straight? (list 10 'Jack 'Queen 'King 'Ace) (list 10 'Jack 'Queen 'King 'Ace)) true)

(define (card-values card)
  (cond
    [(and (integer? card) (not (= card 2))) card]
    [(integer? card) 15]
    [(symbol=? card 'Jack) 11]
    [(symbol=? card 'Queen) 12]
    [(symbol=? card 'King) 13]
    [(symbol=? card 'Ace) 14]
    [(symbol=? card 'Black) 16]
    [else 17]))

(check-expect (card-values 'King) 13)

(define (big-append initial nested)
  (cond
    [(empty? nested) initial]
    [(empty? initial) (big-append (mini-append (first nested) (second nested)) (rest nested))]
    [else (big-append (mini-append initial (first nested)) (rest nested))]))

(check-expect (big-append empty (list
(list (list 1 2 3 4 5) (list 1 2 3 4) (list 1 2 3) (list 1 2) (list 1))
(list (list 2 3 4 5) (list 2 3 4) (list 2 3) (list 2))
(list (list 3 4 5) (list 3 4) (list 3))
(list (list 4 5) (list 4))
(list (list 5)))) (list (list 1 2 3 4 5) (list 1 2 3 4)(list 1 2 3)(list 1 2)
(list 1) (list 2 3 4 5)(list 2 3 4)(list 2 3)(list 2)(list 2 3 4 5)(list 2 3 4)(list 2 3)
(list 2)(list 3 4 5) (list 3 4)(list 3)(list 4 5)(list 4)(list 5)))

(define (mini-append lst1 lst2)
  (cond [(empty? lst1) lst2]
        [else (cons (first lst1) (mini-append (rest lst1) lst2))]))

(check-expect (mini-append (list (list 1 2 3)) (list (list "hi" "cya" "bye")))
             (list (list 1 2 3) (list "hi" "cya" "bye")))

(define (subseqfull hand max)
  (cond
    [(= max 0) empty]
    [else (cons (subseq hand (length hand)) (subseqfull (rest hand) (sub1 max)))]))

(check-expect (subseqfull (list 1 2 3 4 5) 5) (list
 (list (list 1 2 3 4 5) (list 1 2 3 4) (list 1 2 3) (list 1 2) (list 1))
 (list (list 2 3 4 5) (list 2 3 4) (list 2 3) (list 2))
 (list (list 3 4 5) (list 3 4) (list 3))
 (list (list 4 5) (list 4))
 (list (list 5))))

(define (subseq hand max-length)
  (cond
    [(= 0 max-length) empty]
    [else (cons (first-x hand max-length) (subseq hand (sub1 max-length)))]))

(check-expect (subseq (list 1 2 3 4 5) 5)
              (list (list 1 2 3 4 5) (list 1 2 3 4) (list 1 2 3) (list 1 2) (list 1)))

(define (first-x hand x)
  (cond
    [(= x 0) empty]
    [else (cons (first hand) (first-x (rest hand) (sub1 x)))]))

(check-expect (first-x (list 1 2 3 "hi" 'hi 12) 3) (list 1 2 3))
(check-expect (first-x (list 1 2 3 "hi" 'hi 12) 0) (list))
(check-expect (first-x (list 1 2 3 "hi" 'hi 12) 6) (list 1 2 3 "hi" 'hi 12))

;;
;; Problem F
;;

;; (straight-pairs hand) produces a list of all the playable straights with pairs in a hand

;; Examples:
(check-expect
(straight-pairs (list 3 3 3 4 4 5 5 6 6 7 7 9 9 'Jack 'Jack 'Queen 'Queen 'King 'King 2 2 'Black))
(list (list 3 3 4 4 5 5) (list 4 4 5 5 6 6) (list 5 5 6 6 7 7)
(list 'Jack 'Jack 'Queen 'Queen 'King 'King)
(list 3 3 4 4 5 5 6 6) (list 4 4 5 5 6 6 7 7) (list 3 3 4 4 5 5 6 6 7 7)))
      
(check-expect
(straight-pairs (list 3 3 4 4 5 5 6 9 'Jack 'Jack 'Queen 'Queen 'King 'King 'Ace 'Red 'Red))
(list (list 3 3 4 4 5 5) (list 'Jack 'Jack 'Queen 'Queen 'King 'King)))

;; straight-pairs: Hand -> (lstof Hand)
(define (straight-pairs hand) (straight-pairs-b (straight-pairs-a hand)))

;; Tests:
(check-expect
(straight-pairs (list 3 3 3 4 5 5 5 6 6 6 6 7 7 7 8 8 9 'Ace 'Ace))
(list (list 5 5 6 6 7 7) (list 6 6 7 7 8 8)
(list 5 5 6 6 7 7 8 8)))

(check-expect
(straight-pairs (list 3 3 4 4 5 6 8 9 10 'Queen 'Queen 'Queen 'King 'Ace 2 2 2 'Red))
(list))

;; HELPER FUNCTIONS

(define (straight-pairs-b hand)
  (cond
    [(empty? hand) empty]
    [else (cons (dupl 2 2 (first hand)) (straight-pairs-b (rest hand)))]))

(check-expect (straight-pairs-b (list (list 5 6 7) (list 6 7 8) (list 5 6 7 8)))
              (list (list 5 5 6 6 7 7) (list 6 6 7 7 8 8) (list 5 5 6 6 7 7 8 8)))

(define (straight-pairs-a hand)
    (cond
    [(> 6 (length hand)) empty]
  [else (sort-hands (remove-duplicates (bulk-straights-p
                (big-append empty
                     (subseqfull (find-kind 2 hand)
                                 (length (find-kind 2 hand)))))))]))

(check-expect (straight-pairs-a (list 3 3 3 4 5 5 5 6 6 6 6 7 7 7 8 8 9 'Ace 'Ace))
              (list (list 5 6 7) (list 6 7 8) (list 5 6 7 8)))

(define (dupl numI num hand)
  (cond
    [(empty? hand) empty]
    [(> num 0) (cons (first hand) (dupl numI (sub1 num) hand))]
    [else (dupl numI numI (rest hand))]))

(check-expect (dupl 2 2 (list 4 7 3)) (list 4 4 7 7 3 3))
    
(define (straight-p? pot-straight pot-straight-rec)
  (cond
    [(> 3 (length pot-straight)) false]
    [(empty? (rest pot-straight-rec)) true]
    [(or (> (card-values (second pot-straight-rec)) 14)
         (> (card-values (first pot-straight-rec)) 14)) false]
    [(= 1 (- (card-values (second pot-straight-rec)) (card-values (first pot-straight-rec))))
     (and true (straight-p? pot-straight (rest pot-straight-rec)))]
    [else false]))

(check-expect (straight-p? (list 4 5 6) (list 4 5 6)) true)

(define (bulk-straights-p nested)
  (cond
    [(empty? nested) empty]
    [(straight-p? (first nested) (first nested))
     (cons (first nested) (bulk-straights-p (rest nested)))]
    [else (bulk-straights-p (rest nested))]))

(check-expect (bulk-straights-p (list (list 4 5 6) (list 7 8 9 10 'Jack) (list 4 5 6 7 8 9 10)))
(list (list 4 5 6) (list 7 8 9 10 'Jack) (list 4 5 6 7 8 9 10)))
(check-expect (bulk-straights-p (list (list 3 4 5 7 9) (list 10 'Jack 'Queen 'King 'Ace)))
              (list (list 10 'Jack 'Queen 'King 'Ace)))

;;
;; Problem G
;;

;; (airplanes hand) produces a list of all the playable airplanes in a hand

;; Examples:
(check-expect (airplanes (list 3 3 3 3 4 4 4 4))
(list (list 3 3 3 4 4 4)))

(check-expect
(airplanes (list 'Queen 'Queen 'Queen 'Queen 'King 'King 'King 'Ace 'Ace 'Ace 2 2 2))
(list (list 'Queen 'Queen 'Queen 'King 'King 'King) (list 'King 'King 'King 'Ace 'Ace 'Ace)
(list 'Queen 'Queen 'Queen 'King 'King 'King 'Ace 'Ace 'Ace)))

;; airplanes: Hand -> (lstof Hand)
(define (airplanes hand) (airplanes-b (airplanes-a hand)))

;; Tests:
(check-expect
(airplanes (list 3 3 3 4 4 4 10 10 10 'Jack 'Jack 'Jack 'Jack 'Queen 'Queen 'Queen 2 2 2))
(list (list 3 3 3 4 4 4) (list 10 10 10 'Jack 'Jack 'Jack)
(list 'Jack 'Jack 'Jack 'Queen 'Queen 'Queen) (list 10 10 10 'Jack 'Jack 'Jack 'Queen 'Queen 'Queen)))

(check-expect
(airplanes (list 3 3 3 4 4 5 6 8 9 10 'Queen 'Queen 'Queen 'King 'King 'Ace 2 2 2 'Red))
(list))

;; HELPER FUNCTIONS

(define (airplanes-b hand)
  (cond
    [(empty? hand) empty]
    [else (cons (dupl 3 3 (first hand)) (airplanes-b (rest hand)))]))

(check-expect (airplanes-b (list (list 5 6 7) (list 6 7 8) (list 5 6 7 8)))
              (list (list 5 5 5 6 6 6 7 7 7) (list 6 6 6 7 7 7 8 8 8) (list 5 5 5 6 6 6 7 7 7 8 8 8)))

(define (airplanes-a hand)
    (cond
    [(> 6 (length hand)) empty]
  [else (sort-hands (remove-duplicates (bulk-airplanes
                (big-append empty
                     (subseqfull (find-kind 3 hand)
                                 (length (find-kind 3 hand)))))))]))

(check-expect (airplanes-a (list 3 3 3 4 5 5 5 6 6 6 6 7 7 7 8 8 9 'Ace 'Ace))
              (list (list 5 6) (list 6 7) (list 5 6 7)))
    
(define (airplane? pot-plane pot-plane-rec)
  (cond
    [(> 2 (length pot-plane)) false]
    [(empty? (rest pot-plane-rec)) true]
    [(or (> (card-values (second pot-plane-rec)) 14)
         (> (card-values (first pot-plane-rec)) 14)) false]
    [(= 1 (- (card-values (second pot-plane-rec)) (card-values (first pot-plane-rec))))
     (and true (airplane? pot-plane (rest pot-plane-rec)))]
    [else false]))

(check-expect (airplane? (list 4 5 6) (list 4 5 6)) true)

(define (bulk-airplanes nested)
  (cond
    [(empty? nested) empty]
    [(airplane? (first nested) (first nested))
     (cons (first nested) (bulk-airplanes (rest nested)))]
    [else (bulk-airplanes (rest nested))]))

(check-expect (bulk-airplanes (list (list 4 5 6) (list 7 8 9 10 'Jack) (list 4 5 6 7 8 9 10)))
(list (list 4 5 6) (list 7 8 9 10 'Jack) (list 4 5 6 7 8 9 10)))
(check-expect (bulk-airplanes (list (list 3 4 5 7 9) (list 10 'Jack 'Queen 'King 'Ace)))
              (list (list 10 'Jack 'Queen 'King 'Ace)))
