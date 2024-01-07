;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Adam Price (21060015)
;; CS 135 Fall 2023
;; Assignment 02, Problem 3
;; ***************************************************
;;

;;Checking that both the origingal and the
;;simplified functions have the same functionality

(check-expect (q3a -1 true) (q3a-simplified -1 true))
(check-expect (q3a 0 true) (q3a-simplified 0 true))
(check-expect (q3a 1 true) (q3a-simplified 1 true))
(check-expect (q3a -1 false) (q3a-simplified -1 false))
(check-expect (q3a 0 false) (q3a-simplified 0 false))
(check-expect (q3a 1 false) (q3a-simplified 1 false))

;;Original part a funciton
(define (q3a n a?)
  (cond [a? (cond [(>= n 0) (+ n 1)]
                  [else (- n 1)])]
        [else 0]))

;;Simplified part a function
(define (q3a-simplified n a?)
  (cond [(and a? (>= n 0)) (+ n 1)]
        [a? (- n 1)]
        [else 0]))


;;Checking that both the origingal and the
;;simplified functions have the same functionality

(check-expect (q3b true true true) (q3b-simplified true true true))
(check-expect (q3b true true false) (q3b-simplified true true false))
(check-expect (q3b true false false) (q3b-simplified true false false))
(check-expect (q3b false false false) (q3b-simplified false false false))
(check-expect (q3b false true true) (q3b-simplified false true true))
(check-expect (q3b false false true) (q3b-simplified false false true))
(check-expect (q3b true false true) (q3b-simplified true false true))
(check-expect (q3b false true false) (q3b-simplified false true false))

;;Original part b function

(define (q3b a? b? c?)
  (cond
    [a? (cond [b? 'elm]
              [(not c?) 'birch]
              [else 'cedar])]
    [else (cond ; Yuck. else followed by cond!
            [b? 'pine]
            [(not c?) 'birch]
            [else 'cherry])]))

;;Simplified part b function

(define (q3b-simplified a? b? c?)
  (cond
    [(and a? b?) 'elm]
    [(and a? c?) 'cedar]
    [a? 'birch]
    [b? 'pine]
    [c? 'cherry]
    [else 'birch]))

;;Checking that both the origingal and the
;;simplified functions have the same functionality

(check-expect (q3c true true true) (q3c-simplified true true true))
(check-expect (q3c true true false) (q3c-simplified true true false))
(check-expect (q3c true false false) (q3c-simplified true false false))
(check-expect (q3c false false false) (q3c-simplified false false false))
(check-expect (q3c false true true) (q3c-simplified false true true))
(check-expect (q3c false false true) (q3c-simplified false false true))
(check-expect (q3c true false true) (q3c-simplified true false true))
(check-expect (q3c false true false) (q3c-simplified false true false))

;;Original part c function
(define (q3c a? b? c?)
  (cond
    [(cond [c? b?]
           [else (not a?)]) (cond
                              [b? 'spruce]
                              [c? 'fir]
                              [else 'larch])]
    [else (cond [a? 'hazel]
                [else 'hickory])]))

;;simplified part c function
(define (q3c-simplified a? b? c?)
 (cond
    [(and (not a?)(not b?)(not c?)) 'larch]
    [(or (and b? c?) (and (not a?) (not c?))) 'spruce]
    [a? 'hazel]
    [else 'hickory]))
  

