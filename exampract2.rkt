;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exampract2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;1
;;John mcarthy created the language called lisp, the predesessor to racket. This ground breaking
;language supported reccursion. It was and has been a key tool in teaching and researching computer
;science even until this day as noted by our use of racket in this course

;2

;;make-quad-fn: (Num Num Num -> (Num -> Num))
(define (make-quad-fn a b c)
  (lambda (x) (+ (* a (sqr x)) (* b x) c)))

(define (key-count bst key)
  (cond [(empty? bst) 0]
        [(= key (node-key bst)) (+ 1 (key-count (node-left bst) key)(key-count (node-right bst key)))]
        [else (+ (key-count (node-left bst) key)(key-count (node-right bst key)))]))

;3
(filter (lambda (x) (zero? (remainder x 3))) (build-list 3 sqr))
(filter (lambda (x) (zero? (remainder x 3))) (list 0 1 4))
;(list 0)

;b
((lambda y z) (list 1 y z) 3 4)
(list 1 3 4)


;c
(+ (local [(define z (* 2 1))
           (define (y x) (* 3 x))]
     (y z)) 3)

(define z_0 (* 2 1))
(define (y_0 x) (* 3 x))

(+ (y_0 z_0) 3)

;9

(cond [(> (power 100 2) (* 100 90)) (map power (list 100 2 90))]
      [else true]
(cond [(> (expt 100 2) (* 100 90)) (map power (list 100 2 90))]
      [else true]
;;error cannot map power to a list as it requires 2 arguments

;;dir-search: Directory Str Nat -> (listof FileDir)
(define (dir-search dir target acc)
  (cond [(string=? (first dir) target) (file-search (rest dir) target (add1 acc))]
        [else (file-search (rest dir) target acc)]))

      
;;count-name: Directory Str -> Nat
(define (count-name dir target)
  (cond [(string=? target (first dir)) (add1 (file-search (rest dir) target 0))]
        [else (file-search (rest dir) target 0)]))

;;file-search: Directory Str Nat -> Nat
(define (file-search dir target acc)
  (cond [(empty? (rest dir)) acc]
        [(list? (rest (first dir))) (dir-search (first dir) target acc)]
        [(string=? (first dir) target) (file-search (rest dir) target (add1 acc))]
        [else (file-search (rest dir) target acc)]))

;6
;a done already
;b
;Func:
(lambda (x y) (max (abs x) (abs y)))
;Base:
empty

;c
;FUNC:
(lambda (x y) (= 0 (remainder x y)))
;Base
true

;7
(/ (foldr + 0 ds) (length ds))

;b
(define (dataset-stdev ds)
  (local [(define mean (dataset-mean ds))
          (define N (length ds))
          (define sum (foldr (lambda (x rror) (+ (sqr (- x mean))) rror) 0 ds))]
    (sqrt (* (/ 1 (sub1 N)) sum))))
;c
(define (remove-outliers ds)
  (local [(define sd (dataset-stdev ds))
          (define mean (dataset-mean ds))]
    (filter (lambda (x) (or (> x (+ mean (* 3 sd))) (< x (- mean (* 3 sd))))) ds)))

;d
(define (destruction-sort lon)
  (foldr (lambda (x rror) (cond [(< x (min rror)) (cons x rror)]
                                [else rror])) empty lon))

;8
42
(list 4)
9
(lambda (x) (* x 2))
'(())
(list 2 6 12)
(list true false true)
'((pass This) '(pass 2 "shall"))

;9
(define (shift-left n lst)
  (cond [(= n 0) lst]
        [else (shift-left (sub1 n) (append (rest lst) (list (first lst))))]))

;b

(define (shift-right n lst)
  (cond [(= n 0) lst]
        [else (shift-right (sub1 n) (reverse (append (rest (reverse lst))
                                                     (list (first (reverse lst))))))])

  ;c
(define (shift-list loi)
  (cond [(> 0 (first loi)) (shift-list (shift-left (abs (first loi)) loi))]
        [(< 0 (first loi)) (shift-list (shift-right (abs (first loi)) loi))]
        [else loi]))

  ;d
;no, (list 1 1 1 1)


  ;10
  ;a, D F
  ;b, false
  ;c, B D F C E G H
  ;d, C E G H
  

