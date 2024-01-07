;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Adam Price (21060015)
;; CS 135 Fall 2023
;; Assignment 03, Problem 3
;; ***************************************************
;;

;;Question 3a

;; A robot's State (State) is a
;; (cons Sym (cons Nat (cons Nat empty)))
;; requires: each Nat element in the list
;; must be <= 10 and >= 0 and the Sym is one of
;; 'North 'South 'East 'West

;;Question 3b

;;(robot-ctl list-of-inputs command) changes the robot's state based off original state and command

;;Examples
(check-expect
 (robot-ctl (cons 10 (cons 10 (cons 'North empty))) 'forward)
 (cons 10 (cons 10 (cons 'North empty))))
(check-expect
 (robot-ctl (cons 11 (cons 11 (cons 'North empty))) 'turn-left)
 (cons 11 (cons 11 (cons 'West empty))))

;;robot-ctl: State Sym -> State
(define (robot-ctl list-of-inputs command)
  (cond [(symbol=? command 'turn-left)
         (cond [(symbol=? 'North
                (first (rest (rest list-of-inputs))))
                (cons (first list-of-inputs)
                      (cons (first (rest list-of-inputs))
                      (cons 'West empty)))]
               [(symbol=? 'West
                (first (rest (rest list-of-inputs))))
                (cons (first list-of-inputs)
                      (cons (first (rest list-of-inputs))
                      (cons 'South empty)))]
               [(symbol=? 'East
                (first (rest (rest list-of-inputs))))
                (cons (first list-of-inputs)
                      (cons (first (rest list-of-inputs))
                      (cons 'North empty)))]
               [(symbol=? 'South
                (first (rest (rest list-of-inputs))))
                (cons (first list-of-inputs)
                      (cons (first (rest list-of-inputs))
                      (cons 'East empty)))])]
        [(symbol=? command 'turn-right)
         (cond [(symbol=? 'North
                (first (rest (rest list-of-inputs))))
                (cons (first list-of-inputs)
                      (cons (first (rest list-of-inputs))
                      (cons 'East empty)))]
               [(symbol=? 'West
                (first (rest (rest list-of-inputs))))
                (cons (first list-of-inputs)
                      (cons (first (rest list-of-inputs))
                      (cons 'North empty)))]
               [(symbol=? 'East
                (first (rest (rest list-of-inputs))))
                (cons (first list-of-inputs)
                      (cons (first (rest list-of-inputs))
                      (cons 'South empty)))]
               [(symbol=? 'South
                (first (rest (rest list-of-inputs))))
                (cons (first list-of-inputs)
                      (cons (first (rest list-of-inputs))
                      (cons 'West empty)))])]
        [(symbol=? command 'forward)
         (cond [(and (symbol=?
                (first (rest (rest list-of-inputs)))
                'North)
                     (< (first
                         (rest list-of-inputs)) 10))
                (cons (first list-of-inputs)
                (cons (+ 1
                      (first (rest list-of-inputs)))
                (cons (first (rest
                      (rest list-of-inputs))) empty)))]
               [(and (symbol=?
                (first (rest (rest list-of-inputs)))
                'South)
                (> (first (rest list-of-inputs)) 0))
                (cons (first list-of-inputs)
                (cons (- 1 (first (rest list-of-inputs)))
                (cons
                (first (rest
                        (rest list-of-inputs))) empty)))]
               [(and (symbol=?
                (first (rest (rest list-of-inputs)))
                'West)
                     (< 0 (first list-of-inputs)))
                (cons (- 1 (first list-of-inputs))
                (cons (first (rest list-of-inputs))
                (cons (first
                      (rest (rest list-of-inputs)))
                      empty)))]
               [(and (symbol=?
                (first (rest (rest list-of-inputs)))
                'East)
                     (> 10 (first list-of-inputs)))
                (cons (+ 1 (first list-of-inputs))
                      (cons (first (rest list-of-inputs))
                      (cons
                      (first (rest
                      (rest list-of-inputs))) empty)))]
               [else list-of-inputs])]))


;;Tests:
(check-expect
 (robot-ctl (cons 1 (cons 1 (cons 'North empty))) 'forward)
 (cons 1 (cons 2 (cons 'North empty))))
(check-expect
 (robot-ctl (cons 1 (cons 1 (cons 'North empty))) 'turn-left)
 (cons 1 (cons 1 (cons 'West empty))))
(check-expect
 (robot-ctl (cons 1 (cons 1 (cons 'East empty))) 'turn-right)
 (cons 1 (cons 1 (cons 'South empty))))
(check-expect
 (robot-ctl (cons 1 (cons 1 (cons 'East empty))) 'forward)
 (cons 2 (cons 1 (cons 'East empty))))
(check-expect
 (robot-ctl (cons 1 (cons 1 (cons 'West empty))) 'forward)
 (cons 0 (cons 1 (cons 'West empty))))
(check-expect
 (robot-ctl (cons 1 (cons 1 (cons 'South empty))) 'forward)
 (cons 1 (cons 0 (cons 'South empty))))












