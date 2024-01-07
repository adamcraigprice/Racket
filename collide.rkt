;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname collide) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Adam Price (21060015)
;; CS 135 Fall 2023
;; Assignment 03, Problem 4
;; ***************************************************
;;

;;A Sphere is a (cons (cons Num (cons Num (cons Num))) (cons Num empty))
;;represents a sphere where the last num
;;but be positive, the first item being
;;a list of coordinates of the center of the
;;sphere and the last item being it's radius

;;Question 4a

;;(build-sphere original-list) produces the corresponding sphere based off the original list

;;Examples
(check-expect
 (build-sphere (cons 10 (cons 10 (cons 10 (cons 54 empty)))))
 (cons (cons 10 (cons 10 (cons 10 empty))) (cons 54 empty)))
(check-expect
 (build-sphere (cons 10 (cons 10 (cons 10 (cons 54 empty)))))
 (cons (cons 10 (cons 10 (cons 10 empty))) (cons 54 empty)))

;;build-sphere: (list Num Num Num Num) -> (list (list Num Num Num) Num)

(define (build-sphere original-list)
  (cons (cons (first original-list)
              (cons (first (rest original-list))
              (cons (first (rest
                            (rest original-list))) empty)))
  (cons (first (rest (rest
  (rest original-list)))) empty)))

;;Tests
(check-expect (build-sphere (cons 0 (cons 1 (cons 2 (cons 4 empty)))))
              (cons (cons 0 (cons 1 (cons 2 empty))) (cons 4 empty)))
(check-expect (build-sphere (cons 120 (cons 11 (cons 25 (cons 40 empty)))))
              (cons (cons 120 (cons 11 (cons 25 empty))) (cons 40 empty)))
(check-expect (build-sphere (cons 0 (cons 0 (cons 0 (cons 54 empty)))))
              (cons (cons 0 (cons 0 (cons 0 empty))) (cons 54 empty)))

;;Question 4b
;;(valid-sphere? sphere) check whether the inputed sphere list is a valid one

;;Examples

(check-expect (valid-sphere?
               (cons (cons 11 (cons 4 (cons 30 empty)))
                     (cons 54 empty))) true)
(check-expect (valid-sphere?
               (cons (cons 0 (cons 11 (cons 0 empty)))
                     (cons -45 empty))) false)
(check-expect (valid-sphere?
               (cons (cons -35 (cons 57.6 (cons -1.09 empty)))
                     (cons 0 empty))) false)

;;valid-sphere?: Sphere -> Bool

(define (valid-sphere? sphere-list)
  (and (number? (first (first sphere-list)))
       (number? (first (rest (first sphere-list))))
       (number? (first (rest (rest (first sphere-list)))))
       (number? (first (rest sphere-list)))
       (< 0 (first (rest sphere-list)))))

(check-expect (valid-sphere?
               (cons (cons 111 (cons 40 (cons 3 empty)))
                                   (cons 54 empty))) true)
(check-expect (valid-sphere?
               (cons (cons 0 (cons 0 (cons 0 empty)))
                                   (cons -45 empty))) false)
(check-expect (valid-sphere?
               (cons (cons -3.5 (cons 5.6 (cons -1 empty)))
                                   (cons 0 empty))) false)

;;Question 4c

;;(distance-between-points list-of-coordinates-1 list-of-coordinates-2) Calculates the distance 
;;between inputted points

;;Examples
(check-expect (distance-between-points
               (cons 0 (cons 0 (cons 1 empty)))
               (cons 0 (cons 0 (cons 2 empty)))) 1)
(check-expect (distance-between-points
               (cons 6 (cons 8 (cons 0 empty)))
               (cons 3 (cons 4 (cons 0 empty)))) 5)

;;distance-between-points: (listof Num) (listof Num) -> Num
(define (distance-between-points
         list-of-list-of-coordinates-1
         list-of-list-of-coordinates-2)
  (sqrt (+
  (sqr (- (first list-of-list-of-coordinates-1)
       (first list-of-list-of-coordinates-2)))
  (sqr (- (first (rest list-of-list-of-coordinates-1))
       (first (rest list-of-list-of-coordinates-2))))
  (sqr (- (first (rest (rest list-of-list-of-coordinates-1)))
       (first (rest (rest list-of-list-of-coordinates-2)))
       )))))

;;Tests
(check-expect (distance-between-points
               (cons 0 (cons 0 (cons 0 empty)))
               (cons 1 (cons 0 (cons 0 empty)))) 1)
(check-expect (distance-between-points
               (cons 0 (cons 0 (cons 0 empty)))
               (cons 3 (cons 4 (cons 0 empty)))) 5)
(check-expect (distance-between-points
               (cons 1 (cons 5 (cons 10 empty)))
               (cons 1 (cons 5 (cons 10 empty)))) 0)

;;Question 4d

;;(point-in-sphere? list-of-coordinates sphere) determines whether a point is in the sphere

;;Examples
(check-expect (point-in-sphere?
               (cons 1 (cons 2 (cons 10 empty)))
               (cons (cons 0 (cons 0 (cons 0 empty)))
                     (cons 54 empty))) true)
(check-expect (point-in-sphere?
               (cons 1 (cons 5 (cons 100 empty)))
               (cons (cons 0 (cons 0 (cons 0 empty)))
                     (cons 1 empty))) false)

;;point-in-sphere?: (listof Num) Sphere -> Bool
(define (point-in-sphere?
         list-of-coordinates sphere)
  (cond [(<= (distance-between-points
             list-of-coordinates
             (first sphere))
             (first (rest sphere))) true]
        [else false]))

;;Tests
(check-expect (point-in-sphere?
               (cons 1 (cons 5 (cons 10 empty)))
               (cons (cons 0 (cons 0 (cons 0 empty)))
                     (cons 54 empty))) true)
(check-expect (point-in-sphere?
               (cons 1 (cons 5 (cons 10 empty)))
               (cons (cons 0 (cons 0 (cons 0 empty)))
                     (cons 1 empty))) false)
  
;;Question 4e
;;(collide? sphere-1 sphere-2) determines whether two sphere overlap/collide

;;Examples
(check-expect (collide?
               (cons (cons 0 (cons 0 (cons 1 empty)))
                     (cons 1 empty))
               (cons (cons 0 (cons 0 (cons 1 empty)))
                     (cons 1 empty))) true)
(check-expect (collide?
               (cons (cons 0 (cons 0 (cons 0 empty)))
                     (cons 1 empty))
               (cons (cons 11 (cons 10 (cons 1000 empty)))
                     (cons 5 empty))) false)

;;collide?: Sphere Sphere -> Bool
(define (collide? sphere-1 sphere-2)
  (cond [(<= (distance-between-points
             (first sphere-1)
             (first sphere-2))
             (+ (first (rest sphere-1))
                (first (rest sphere-2)))) true]
        [else false]))

;;Tests
(check-expect (collide?
               (cons (cons 0 (cons 0 (cons 0 empty)))
                     (cons 1 empty))
               (cons (cons 0 (cons 0 (cons 0 empty)))
                     (cons 1 empty))) true)
(check-expect (collide?
               (cons (cons 0 (cons 0 (cons 0 empty)))
                     (cons 1 empty))
               (cons (cons 11 (cons 10 (cons 10 empty)))
                     (cons 5 empty))) false)