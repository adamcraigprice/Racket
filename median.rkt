;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname median) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Adam Price (21060015)
;; CS 135 Fall 2023
;; Assignment 02, Problem 2
;; ***************************************************
;;

;;Tests for the funciton
(check-expect (median-of-3-simple 10 20 30) 20)
(check-expect (median-of-3-simple 345 333 420) 345)
(check-expect (median-of-3-simple 6000 8000 700) 6000)
(check-expect (median-of-3-simple -3.2 5.8 -1) -1)

;;Simplified median function
(define (median-of-3-simple a b c)
  (cond
    [(<= a b) (cond
                [(<= b c) b]
                [(>= c a) c]
                [else a])]
    [else (cond
                [(<= c b) b]
                [(>= c a) a]
                [else c])]))


