;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname div-by-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;A Nat3 is defined as either 0, 1 or 2 and as any number 3 larger than another Nat3


;;(nat3-template) template for nat-3 definition that checks whether a number is a Nat
;;nat3-template: Num -> Bool
(define (nat3-template nat)
  (cond [(> 0 nat) ...]
        [(or (= nat 0) (= nat 1) (= nat 2)) ...]
        [else (nat3-template (- nat 3))])
  )

;;Examples
(check-expect (div-by-3? 99) true)
(check-expect (div-by-3? 91) false)

;;(div-by-3? nat) check if a number is divisible by 3
;;div-by-3?: Nat3 -> Bool
(define (div-by-3? nat)
  (cond [(> 0 nat) false]
        [(= nat 0) true]
        [else (div-by-3? (- nat 3))])
  )

;;Tests
(check-expect (div-by-3? 3) true)
(check-expect (div-by-3? 9) true)
(check-expect (div-by-3? 12) true)
(check-expect (div-by-3? 11) false)
(check-expect (div-by-3? 23) false)
(check-expect (div-by-3? 13) false)