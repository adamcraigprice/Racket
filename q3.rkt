;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))

;;
;; ***************************************************
;; Adam Price (21060015)
;; CS 135 Fall 2023
;; Assignment 09, Problem 3
;; ***************************************************
;;

;;a
;;Examples

(check-expect (occ (list 1 2 3 4 5 6) 6) 1)
(check-expect (occ (list 6 6 6 6 6 6) 6) 6)

;;(occ lst num) checks how many of a certain number are in a list
;;occ: listof Num Num -> Nat
(define (occ lst num)
  (length (filter (lambda (number) (= num number)) lst)))

;;Tests
(check-expect (occ (list 1 2 3 2 2 6) 2) 3)
(check-expect (occ (list 1 2 1 1 5 1) 1) 4)
(check-expect (occ (list 1 2 1 1 5 1) 100) 0)

;;b
;;Examples
(check-expect (pocket-change (list 'dime 'dime 'dime 'dime 'dime 'dime 'dime)) 0.7)
(check-expect (pocket-change (list 'dime 'dime 'dime 'dime 'dime 'dime 'duyk)) 0.6)

;;(pocket-change lst) adds the symbols that represent coins
;;pocket-change; listof Sym -> Num
(define (pocket-change lst)
  (local [(define (value val)
            (cond [(symbol=? val 'nickel) 0.05]
                  [(symbol=? val 'dime) 0.10]
                  [(symbol=? val 'quarter) 0.25]
                  [(symbol=? val 'loonie) 1]
                  [(symbol=? val 'toonie) 2]
                  [else 0]))]
    (foldr + 0 (map value lst))))

;;Tests
(check-expect (pocket-change empty) 0)
(check-expect (pocket-change (list 'nickel 'dime 'quarter 'loonie 'toonie)) 3.4)
(check-expect (pocket-change (list 'nickel 'dime 'quarter 'loonie 'toonie
                                   'nickel 'dime 'quarter 'loonie 'toonie)) 6.8)
;;c

;; A Matrix is one of:
;; * empty
;; * (cons (listof Num) Matrix)
;; requires: each (listof Num) is non-empty and has the same length

;;Examples
(check-expect (first-col '((1 2 3 4) (5 6 7 8) (9 10 11 12))) (list 1 5 9))
(check-expect (first-col '((1 0) (5 6) )) (list 1 5))

;;(first-col mat) outputs list of first col of a matric
;;first-col: Matrix -> listof Num
(define (first-col mat)
  (cond [(empty? mat) empty]
        [else (map (lambda (x) (cond [(list? x) (first x)]
                         [else x])) mat)]))

;;Tests
(check-expect (first-col '((1))) (list 1))
(check-expect (first-col empty) empty)

;;d
;;Examples
(check-expect (add1-mat '((1 1 1 1) (2 2 2 2) (3 3 3 3)))
(list (list 2 2 2 2) (list 3 3 3 3) (list 4 4 4 4)))
(check-expect (add1-mat '((-8 -8 -8 -7) (2 2 2 2) (3 3 3 3)))
(list (list -7 -7 -7 -6) (list 3 3 3 3) (list 4 4 4 4)))

;;(add1-mat mat) adds one to every number in a matrix
;;add1-matrix: Matrix -> Matrix
(define (add1-mat mat)
  (map (lambda (lst) (first (list (map add1 lst)))) mat))

;;Tests
(check-expect (add1-mat '((1 2 3 4) (5 6 7 8) (9 10 11 12)))
(list (list 2 3 4 5) (list 6 7 8 9) (list 10 11 12 13)))
(check-expect (add1-mat '((4 3 2 1) (5 6 7 8) (9 10 11 12)))
(list (list 5 4 3 2) (list 6 7 8 9) (list 10 11 12 13)))
(check-expect (add1-mat '((4 3 2 1)))
(list (list 5 4 3 2)))

;;e

;;Examples
(check-expect (sum-at-zero (list (lambda (x) (* 2 x)) (lambda (x) (+ 2 x)) sqr abs add1)) 3)
(check-expect (sum-at-zero (list sqr sqr sqr sqr sqr)) 0)

;;(sum-at-zero flist) sums the application of a list of function to 0
;;sum-at-zero: listof (Num -> Num) -> Num
(define (sum-at-zero flist)
  (cond [(empty? flist) 0]
        [else (foldl + 0 (map (lambda (f) (f 0)) flist))]))

;;Tests
(check-expect (sum-at-zero (list add1 sqr abs add1)) 2)
(check-expect (sum-at-zero (list add1 sqr sub1)) 0)
(check-expect (sum-at-zero (list add1 sqr sub1 add1 sqr add1 add1 sqr add1)) 4)
(check-expect (sum-at-zero (list)) 0)




