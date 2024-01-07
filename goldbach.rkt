;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname goldbach) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))

;;
;; ***************************************************
;; Adam Price (21060015)
;; CS 135 Fall 2023
;; Assignment 10, Problem 2
;; ***************************************************
;;
;;a

;;Examples
(check-expect (primes 29) '(2 3 5 7 11 13 17 19 23 29))
(check-expect (primes 101) '(2 3 5 7 11 13 17 19 23 29
                               31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101))

;;(primes p) displays all primes from 2 to p
;;primes: Nat -> listof Nat
(define (primes p) 
  (local [(define (primes-main p lst)
            (cond [(empty? lst) empty]
                  [else (cons (first lst) (primes-main p
                  (filter (lambda (x) (not (= (modulo x (first lst)) 0))) lst)))]))]
    (primes-main p (build-list (sub1 p) (lambda (x) (+ 2 x))))))

;;Tests
(check-expect (primes 21) '(2 3 5 7 11 13 17 19))
(check-expect (primes 30) '(2 3 5 7 11 13 17 19 23 29))
(check-expect (primes 100) '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))
(check-expect (primes 2) '(2))

;b
;;Examples
(check-expect (goldbach 66) 6)
(check-expect (goldbach 100) 6)


;;(goldbach n) calcs the number of ways a given number can be written as the sum of 2 primes
;;goldbach: Nat -> Nat
(define (goldbach n)
  (local [(define prime (primes n))]
  (foldr (lambda (x rror) (cond [(> x (/ n 2)) 0]
                                [(member? (- n x) prime) (add1 rror)]
                                [else rror])) 0 prime)))

;;Test
(check-expect (goldbach 12) 1)
(check-expect (goldbach 14) 2)
(check-expect (goldbach 1000) 28)
(check-expect (goldbach 10000) 127)

