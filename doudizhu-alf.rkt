;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname doudizhu-alf) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))





;; A CardCountAL (CCAL) is one of:
;; * empty
;; * (cons (list Card Nat) CardCountAL)
;; Requires:
;; Card is a unique key and Nat > 0

;;a
(define (hand->ccal hand)
  (local [(define (card=? c1 c2) (cond [(and (symbol? c1) (symbol? c2) (symbol=? c1 c2)) true]
                                       [(and (integer? c1) (integer? c2) (= c1 c2)) true]
                                       [else false]))
    (define (split lst) (reverse (foldl (lambda (x rror)
                                          (cond [(foldr (lambda (y ac)
                                                          (cond [(card=? x y) true]
                                                                [else ac])) false rror)rror]
                                                [else (append (list x) rror)])) (list) lst)))
    (define (count c l)
      (foldr (lambda (x rror) (cond [(card=? x c) (+ 1 rror)]
                                    [else rror])) 0 l))]
    (map (lambda (x) (list x (count x hand))) (split hand))))
      

;;b
(define (find-kind n h)
  (map first (filter (lambda (x) (<= n (first (rest x)))) h)))


; (list (list 5 '3) (list 2 '4) (list 5 '5))
(check-expect (find-kind 3 (list (list 3 5) (list 2 4) (list 5 4))) (list 3 2 5))
(check-expect (find-kind 3 (list (list 'Jack 5) (list 2 5) (list 'Red 5))) (list 'Jack 2 'Red))

;c
(define (trios h)
  (map (lambda (x) (build-list 3 (lambda (z) (first x))))
       (filter (lambda (x) (>= (first (rest x)) 3)) h)))

(check-expect (trios (list (list 5 '3) (list 2 '4) (list 5 '5))) (list (list 5 5 5) (list 2 2 2)
                                                                       (list 5 5 5)))


;d
;; card-foldr-n: (Card Nat X -> X) Card X Nat -> X
;; Requires: Nat >= 0
(define (card-foldr-n combine card base n)
  (cond [(zero? n) base]
        [else (combine card n (card-foldr-n combine card base (sub1
           n)))]))

(define (ccal->hand l)
  (local
    [(define (card-foldr-n combine card base n)
       (cond [(zero? n) base]
             [else (combine card n (card-foldr-n combine card base (sub1 n)))]))]
    (foldr (lambda (x rror) (append (card-foldr-n (lambda (x y rror) (cons x rror))
                                                  (first x) empty (first (rest x))) rror)) empty l)))

(check-expect (ccal->hand (list (list 2 2) (list 'Jack 2) (list 'Red 2)))
              (list 2 2 'Jack 'Jack 'Red 'Red))

;;e 
(define (remove-one-of-each hand)
  (foldr (lambda (x y rror) (cond [((lambda (k j) (cond
                                          [(and (symbol? k) (symbol? j) (symbol=? k j)) true]
                                          [(and (integer? k) (integer? j) (= k j)) true]
                                          [else false])) x y) (cons x rror)] [else rror]))
         empty hand (append (rest hand) (list 'end))))

(check-expect (remove-one-of-each (list 2 2 'Jack 'Jack 'Red 'Red))
              (list 2 'Jack 'Red) )

;;f
(define (remove-one-of-each2 hand)
  (ccal->hand (map (lambda (x) (list (first x) (sub1 (first (rest x))))) (hand->ccal hand))))


(check-expect (remove-one-of-each2 (list 2 2 'Jack 'Jack 'Red 'Red))
              (list 2 'Jack 'Red) )

