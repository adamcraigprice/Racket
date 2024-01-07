;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cardgame) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;Question 3a
(define card-list (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 (cons 10 (cons 'Jack
            (cons 'Queen (cons 'King (cons 'Ace (cons 2 (cons 'Black (cons 'Red empty))))))))))))))))

(define (card? possible-card)
  (member possible-card card-list))

;;Tests
(check-expect (card? 3) true)
(check-expect (card? 4) true)
(check-expect (card? 5) true)
(check-expect (card? 6) true)
(check-expect (card? 7) true)
(check-expect (card? 8) true)
(check-expect (card? 9) true)
(check-expect (card? 10) true)
(check-expect (card? 11) false)
(check-expect (card? 'Jack) true)
(check-expect (card? 'King) true)
(check-expect (card? 'Queen) true)
(check-expect (card? 'Ace) true)
(check-expect (card? 2) true)
(check-expect (card? 'Red) true)
(check-expect (card? 'Black) true)
(check-expect (card? 'nfienof) false)


;;Examples
(check-expect (card=? 3 3) true)
(check-expect (card=? 'Orange 'Orange) false)

;;(card=? card-1 card-2) checks whether inputs are cards and check if they are the same one
;;

(define (card=? card-1 card-2)
  (cond [(and (card? card-1) (card? card-2))
         (member card-2 (cons card-1 empty))]
        [else false]))

;;Tests
(check-expect (card=? 'Black 'Black) true)
(check-expect (card=? 'Black 'Red) false)
(check-expect (card=? 3 3) true)
(check-expect (card=? 'Orange 'Orange) false)



;;Question 3b

(define (past-value-of-cards card list-of-cards)
      (cond [(empty? list-of-cards) 0]
            [else (past-value-of-cards card (rest list-of-cards))]))

(define (value-of-cards card list-of-cards)
  (cond [(empty? list-of-cards) 0]
        [(card=? (first list-of-cards) card)
         (+ 1 (past-value-of-cards card (rest list-of-cards)))]
        [else (+ 1 (value-of-cards card (rest list-of-cards)))]))

;;Tests
(check-expect (value-of-cards 3 card-list) 1)
(check-expect (value-of-cards 'Queen card-list) 10)
(check-expect (value-of-cards 4 card-list) 2)

;;Using the insertion sort code provided to us I implement these helper functions 


;; (insert n slon) inserts the number n into the sorted list slon
;;     so that the resulting list is also sorted.
;; Example:
(check-expect (insert 4 (cons 3 (cons 5 (cons 5 empty))))
              (cons 3 (cons 4 (cons 5 (cons 5 empty)))))

;; insert: Num (listof Num) -> (listof Num)
;; Requires: slon is sorted in non-decreasing order
(define (insert n slon)
  (cond [(empty? slon) (cons n empty)]
        [(<= (value-of-cards n card-list) (value-of-cards (first slon) card-list)) (cons n slon)]
        [else (cons (first slon) (insert n (rest slon)))]))

;; (sort-cards lon) sorts the elements of lon in non-decreasing order
;; Example:
(check-expect (sort-cards (cons 3 (cons 4 (cons 3 (cons 5 (cons 3 empty))))))
              (cons 3 (cons 3 (cons 3 (cons 4 (cons 5 empty))))))

;; sort: (listof Num) -> (listof Num)
(define (sort-cards lon)
  (cond [(empty? lon) empty]
        [else (insert (first lon) (sort-cards (rest lon)))]))

;;Tests for sort-cards 
(check-expect (sort-cards
(list 'King 6 7 'Jack 'Queen 2 7 3 7 3 'Ace 'Jack 2 3 4 5))
(list 3 3 3 4 5 6 7 7 7 'Jack 'Jack 'Queen 'King 'Ace 2 2))

(check-expect (sort-cards
(list 4 4 4 4 4 4 3 4 4))
(list 3 4 4 4 4 4 4 4 4))


;;3c
(define (already-removed sorted-card-list)
  (cond [(empty? (rest sorted-card-list)) sorted-card-list]
        [(card=? (first sorted-card-list) (first (rest sorted-card-list)))
                 (cons (first sorted-card-list (rest (already-removed sorted-card-list))))]
        [else sorted-card-list]))

(define (remove-one-of-each sorted-card-list)
  (cond [(empty? (rest sorted-card-list)) sorted-card-list]
        [
         ]))





































