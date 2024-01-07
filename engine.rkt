;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname engine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))


;;
;; ***************************************************
;; Adam Price (21060015)
;; CS 135 Fall 2023
;; Assignment 10, Problem 3
;; ***************************************************
;;

(require "players.rkt")

;;Examples
(define hand0
'(3 3 3 3 4 5 6 7 7 7 9 9 Jack Jack Queen King 2 2 Black Red))
(define hand1
'(4 4 4 5 5 6 6 7 8 9 10 Jack Queen King Ace 2 2))

(define hand2
'(5 6 8 8 8 9 10 10 10 Jack Queen Queen King King Ace Ace Ace))
(check-expect
(doudizhu (list goldfish goldfish goldfish) (list hand0 hand1 hand2))
'Left)
(check-expect
(doudizhu (list reckless goldfish goldfish) (list hand0 hand1 hand2))
'Landlord)


;;(doudizhu players hands) game engine for doudizhu
;;doudizhu: listof Players listof Hands -> anyof 'Landlord 'Right 'Left  
(define (doudizhu players hands)
  (local[(define (who-turn turn)  
           (cond  [(symbol=? turn 'Landlord) (list first turn 'Right)]
                  [(symbol=? turn 'Right) (list second turn 'Left)]
                  [(symbol=? turn 'Left) (list third turn 'Lanlord)]))
         (define (hand-played player prev a) 
           (cond [(symbol=? player 'Landlord)
                  (cons a (rest prev))]
                 [(symbol=? player 'Right)
                  (list (first prev) a (third prev))]
                 [else (list (first prev) (second prev) a)]))
         (define (remove-hand hand to-rem)
           (cond [(empty? hand) '()]
                 [(member? (first hand) to-rem) (remove-hand (rest hand) to-rem)] 
                 [else (cons (first hand) (remove-hand (rest hand) to-rem))]))
         (define (main-engine players-store hands2 played-hands)
           (local [(define n-hand (remove-hand ((first players-store) hands2) played-hands))
                   (define upd-hand (((first players-store) players)
                                     ((first players-store) hands2)
                                     (second players-store) played-hands))]
             (cond [(empty? n-hand) (second players-store)]
                   [else (main-engine (who-turn (third players-store))
                                      (hand-played (second players-store) hands2 n-hand)
                                      (cons upd-hand played-hands))])))]
    (main-engine (who-turn 'Landlord) hands empty)))
 
;;Tests

(check-expect
(doudizhu (list cautious reckless goldfish) (list hand0 hand1 hand2))
'Landlord)















