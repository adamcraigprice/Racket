;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname examples-a10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;Examples

;q1

;a
(check-expect (adj->edge '((A (C D E)) (B (E J)) (C ()) (D (F J))
(E (K)) (F (K H)) (H ()) (J (H)) (K ())))
              '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
(E K) (F K) (F H) (J H)))

(check-expect (adj->edge '((A (D E)) (B (E J)) (C ()) (D (F J))
(E (K)) (F (K H)) (H ()) (J (H)) (K ())))
              '((A D) (A E) (B E) (B J) (D F) (D J)
(E K) (F K) (F H) (J H)))

(check-expect (adj->edge '((A (B)) (B (C)) (C (D)) (D (E)))) '((A B) (B C) (C D) (D E)))

;b

(check-expect (neighbours 'A '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
(E K) (F K) (F H) (J H))) '(C D E))

(check-expect (neighbours 'B '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
(E K) (F K) (F H) (J H))) '(E J))

(check-expect (neighbours 'C '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
(E K) (F K) (F H) (J H))) '())


;c
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

;2

;a
(check-expect (primes 21) '(2 3 5 7 11 13 17 19))

(check-expect (primes 30) '(2 3 5 7 11 13 17 19 23 29))

(check-expect (primes 100) '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))


;b

(check-expect (goldbach 12) 1)
(check-expect (goldbach 14) 2)
(check-expect (goldbach 66) 6)
(check-expect (goldbach 100) 6)
(check-expect (goldbach 10000) 127)

