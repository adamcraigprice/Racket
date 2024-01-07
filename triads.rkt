;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname triads) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Adam Price (21060015)
;; CS 135 Fall 2023
;; Assignment 02, Problem 5
;; ***************************************************
;;

;;Test examples for the function
(check-expect (triad-type 'a 'c 'e) 'Minor)
(check-expect (triad-type 'd 'f# 'a) 'Major)
(check-expect (triad-type 'a 'c 'd#) 'Diminished)
(check-expect (triad-type 'd 'f# 'a#) 'Augmented)

;;Helper funciton to convert pitch to natural numbers
(define (pitch-to-num pitch)
  (cond
    [(symbol=? pitch 'c) 1]
    [(symbol=? pitch 'c#) 2]
    [(symbol=? pitch 'd) 3]
    [(symbol=? pitch 'd#) 4]
    [(symbol=? pitch 'e) 5]
    [(symbol=? pitch 'f) 6]
    [(symbol=? pitch 'f#) 7]
    [(symbol=? pitch 'g) 8]
    [(symbol=? pitch 'g#) 9]
    [(symbol=? pitch 'a) 10]
    [(symbol=? pitch 'a#) 11]
    [(symbol=? pitch 'b) 12]))

;;Main triad type function that takes 3 symbols part of one of the 4 triad types
(define (triad-type note-one note-two note-three)
  (cond [(or (= (- (pitch-to-num note-one) (pitch-to-num note-two)) 3)
             (= (- (pitch-to-num note-one) (pitch-to-num note-two)) 9))
         (cond [(or (= (- (pitch-to-num note-one) (pitch-to-num note-three)) 7)
               (= (- (pitch-to-num note-one) (pitch-to-num note-three)) 5)) 'Minor]
               [else 'Diminished])]
        [else (cond [(or (= (- (pitch-to-num note-one) (pitch-to-num note-three)) 7)
               (= (- (pitch-to-num note-one) (pitch-to-num note-three)) 5)) 'Major]
               [else 'Augmented])]))
