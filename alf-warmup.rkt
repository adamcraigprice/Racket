;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname alf-warmup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))

;;
;; ***************************************************
;; Adam Price (21060015)
;; CS 135 Fall 2023
;; Assignment 09, Problem 2
;; ***************************************************
;;

;;a
(define (absolutely-odd lst)
  (foldl + 0 (map abs (filter odd? lst))))

(check-expect (absolutely-odd '(1 -5 4 6 5)) 11)
(check-expect (absolutely-odd '()) 0)

;;b

(define (zip l1 l2)
 (foldr (lambda (l1 l2 e) (cons (list l1 l2) e)) empty l1 l2))

(check-expect (zip (list 1 2 3 4) (list "a" "b" "c" "d"))
(list (list 1 "a") (list 2 "b") (list 3 "c") (list 4 "d")))
(check-expect (zip empty empty) empty)

(check-expect (zip (list 4 3 2 1) (list "a" "b" "c" "d"))
(list (list 4 "a") (list 3 "b") (list 2 "c") (list 1
"d")))
(check-expect (zip empty empty) empty)


;;c

(define (unzip l)
  (cons (foldr (lambda (l1 e) (cons (first l1) e)) empty l)
        (cons (foldr (lambda (l1 e) (cons (second l1) e)) empty l) empty)))

(check-expect (unzip '((1 a)(2 b)(3 c))) '((1 2 3) (a b c)))
(check-expect (unzip '()) '(()()))


;;d

(define (dedup l)
  (reverse (foldl (lambda (x rror) (cond [(empty? (filter (lambda (q) (= x q)) rror)) (cons x rror)]
                       [else rror])) empty l)))

(check-expect (dedup '(1 2 1 3 3 2 4)) '(1 2 3 4))
(check-expect (dedup '(4 1 2 1 3 3 2 4)) '(4 1 2 3))
(check-expect (dedup '(100 80 100 80 60 60 60 70 100)) '(100 80 60 70))

;;e

(define (zero-fill str)
  (list->string
   (append (build-list (- 20 (length (string->list str))) (lambda (x) #\0))  (string->list str))))

(check-expect (zero-fill "abcdefghijklmn") "000000abcdefghijklmn")
(check-expect (zero-fill "he00llo") "0000000000000he00llo")


;;f

(define (subsequence lst from to)
  (foldr (lambda (a b rror) (cond [(> from b) rror] [(and (>= b from) (> to b)) (cons a rror)]
                             [else '()])) '() lst (build-list (length lst) (lambda (a) a))))

(check-expect (subsequence (list 1 2 3 4 5 6) 3 5) (list 4 5))
(check-expect (subsequence (list 1 2 3 4 5 6) 0 6) (list 1 2 3 4 5 6))
