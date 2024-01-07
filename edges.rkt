;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname edges) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;
;; ***************************************************
;; Adam Price (21060015)
;; CS 135 Fall 2023
;; Assignment 10, Problem 1
;; ***************************************************
;;

;a

;;Examples
(check-expect (adj->edge '((A (C D E)) (B (E J)) (C ()) (D (F J))
(E (K)) (F (H)) (H ()) (J (H)) (K ())))
              '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
(E K) (F H) (J H)))
(check-expect (adj->edge '((A (C D E F G))))
              '((A C) (A D) (A E) (A F) (A G)))
 
;;(adj->edge adj-g) conerts ajacency list to edge list
;;adj->edge: (listof (list Node (listof Node))) -> listof (listof Node Node) 
(define (adj->edge adj-g)
  (local [(define (remove-dup lst)
            (foldr (lambda (x rror) (cond [(member? x rror) rror]
                                          [else (cons x rror)])) empty lst))
          (define (main-reccur adj-g)
            (foldr (lambda (x rror)
                       (cond [(empty? (second x)) rror]
                             [else (foldr (lambda (y rror2)
                                            (cons (list (first x) y) rror2))
                                          empty (second x))])) empty adj-g))
          (define (reccurse adj-g)  
            (cond [(empty? adj-g) empty]
                  [else (append (main-reccur (rest adj-g))(reccurse (rest adj-g)))]))] 
  (remove-dup (append (main-reccur adj-g) (reccurse adj-g)))))


;;Tests
(check-expect (adj->edge '((A (C D E)) (B (E J)) (C ()) (D (F J))
(E (K)) (F (K H)) (H ()) (J (H)) (K ())))
              '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
(E K) (F K) (F H) (J H)))

(check-expect (adj->edge '((A (D E)) (B (E J)) (C ()) (D (F J))
(E (K)) (F (K H)) (H (B)) (J (H)) (K ()) (W ())))
              '((A D) (A E) (B E) (B J) (D F) (D J)
(E K) (F K) (F H) (H B) (J H)))

(check-expect (adj->edge '((A (B)) (B (C)) (C (D)) (D (E)))) '((A B) (B C) (C D) (D E)))

;b
;;Examples
(check-expect (neighbours 'E '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
(E K) (F K) (F H) (J H))) '(K))

(check-expect (neighbours 'B '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
(E K) (F K) (F H) (J H) (B H) (B W))) '(E J H W))

;;(neighbours v edge-g) produces list of all the out neighbours of v
;;neighbours: Node listof (listof Node Node) -> listof Node
(define (neighbours v edge-g)
  (foldr (lambda (x rror) (cond [(symbol=? v (first x)) (cons (second x) rror)]
                                [else rror])) empty edge-g))
;;Tests
(check-expect (neighbours 'A '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
(E K) (F K) (F H) (J H))) '(C D E))

(check-expect (neighbours 'B '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
(E K) (F K) (F H) (J H))) '(E J))

(check-expect (neighbours 'C '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
(E K) (F K) (F H) (J H))) '())


;;c

;;Examples
(check-expect (edge->adj '((A B) (B C) (C D) (D A))) (list
 (list 'A (list 'B))
 (list 'B (list 'C))
 (list 'C (list 'D))
 (list 'D (list 'A))))
(check-expect (edge->adj '((A B) (B A) (C D) (D A))) (list
 (list 'A (list 'B))
 (list 'B (list 'A))
 (list 'C (list 'D))
 (list 'D (list 'A))))

;;(edge->adj edge-g) converts edge list representation to ajacency list representation
;;edge->adj: listof (listof Node Node) -> (listof (list Node (listof Node))) 
(define (edge->adj edge-g)
  (local [(define (intital-node edge-g visited)
            (cond [(empty? edge-g) empty]
                  [(member? (first (first edge-g)) visited) (intital-node2 edge-g visited)]
                  [else (cons (list (first (first edge-g)) (neighbours (first (first edge-g)) edge-g))
                              (intital-node2 edge-g (cons (first (first edge-g)) visited)))]))
          (define (intital-node2 edge-g visited)
            (cond [(empty? edge-g) empty]
                  [(member? (second (first edge-g)) visited) (intital-node (rest edge-g) visited)]
                  [else (cons (list (second (first edge-g)) (neighbours (second (first edge-g)) edge-g))
                              (intital-node (rest edge-g) (cons (second (first edge-g)) visited)))]))]
    (intital-node edge-g empty)))

;;Tests
(check-expect (edge->adj '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
(E K) (F K) (F H) (J H))) (list
 (list 'A (list 'C 'D 'E))
 (list 'C '())
 (list 'D (list 'F 'J))
 (list 'E (list 'K))
 (list 'B (list 'E 'J))
 (list 'J (list 'H))
 (list 'F (list 'K 'H))
 (list 'K '())
 (list 'H '())))

(check-expect (edge->adj
              '((A D) (A E) (B E) (B J) (D F) (D J)
(E K) (F K) (F H) (J H))) (list
 (list 'A (list 'D 'E))
 (list 'D (list 'F 'J))
 (list 'E (list 'K))
 (list 'B (list 'E 'J))
 (list 'J (list 'H))
 (list 'F (list 'K 'H))
 (list 'K '())
 (list 'H '())))

(check-expect (edge->adj '((A B) (B C) (C D) (D E))) (list
 (list 'A (list 'B))
 (list 'B (list 'C))
 (list 'C (list 'D))
 (list 'D (list 'E))
 (list 'E '())))






