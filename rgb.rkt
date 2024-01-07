;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rgb) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Adam Price (21060015)
;; CS 135 Fall 2023
;; Assignment 03, Problem 2
;; ***************************************************
;;

;; An RGB Triplet (RGB) is a (cons Nat (cons Nat (cons Nat empty)))
;; requires: each element in the list must be <= 255

;;Question 2a
;;(RGB->name list-of-ints) produces the name of the inputted rgb colour
;;Examples
(check-expect(RGB->name (cons 255 (cons 255
                                        (cons 255 empty)))) 'white)
(check-expect(RGB->name (cons 255 (cons 0
                                        (cons 0 empty)))) 'red)
(check-expect(RGB->name (cons 0 (cons 255
                                      (cons 0 empty)))) 'green)

;;RGB->name: RGB -> Sym

(define (RGB->name list-of-ints)
  (cond [(and (or (= (first list-of-ints) 255)
             (= (first list-of-ints) 0))
              (or (= (first (rest list-of-ints)) 255)
             (= (first (rest list-of-ints)) 0))
              (or (= (first (rest (rest list-of-ints))) 255)
             (= (first (rest (rest list-of-ints))) 0)))
  (cond [(= (first list-of-ints) 255)
         (cond [(= (first (rest list-of-ints)) 255)
               (cond [(= (first (rest (rest list-of-ints))) 255) 'white]
                     [else 'yellow])]
               [(= (first (rest (rest list-of-ints))) 255) 'magenta]
               [else 'red])]
        [(= (first (rest list-of-ints)) 255)
               (cond [(= (first (rest (rest list-of-ints))) 255) 'cyan]
                     [else 'green])]
        [(= (first (rest (rest list-of-ints))) 255) 'blue]
        [(= (first (rest (rest list-of-ints))) 0) 'black])]
        [else 'unknown]))

;; Tests:

(check-expect(RGB->name (cons 255 (cons 255 (cons 255 empty)))) 'white)
(check-expect(RGB->name (cons 255 (cons 0 (cons 0 empty)))) 'red)
(check-expect(RGB->name (cons 0 (cons 255 (cons 0 empty)))) 'green)
(check-expect(RGB->name (cons 0 (cons 0 (cons 255 empty)))) 'blue)
(check-expect(RGB->name (cons 0 (cons 0 (cons 0 empty)))) 'black)
(check-expect(RGB->name (cons 255 (cons 255 (cons 0 empty)))) 'yellow)
(check-expect(RGB->name (cons 255 (cons 0 (cons 255 empty)))) 'magenta)
(check-expect(RGB->name (cons 0 (cons 255 (cons 255 empty)))) 'cyan)
(check-expect(RGB->name (cons 255 (cons 111 (cons 235 empty)))) 'unknown)

;;Question 2b
;;(name->RGB colour) produces the a list of rgb colour values

;;Examples
(check-expect(name->RGB 'white) (cons 255 (cons 255 (cons 255 empty))))
(check-expect(name->RGB 'red) (cons 255 (cons 0 (cons 0 empty))))
(check-expect(name->RGB 'green) (cons 0 (cons 255 (cons 0 empty))))

;;name->RGB: Sym -> RGB

(define (name->RGB colour)
  (cond [(symbol=? colour 'red) (cons 255 (cons 0 (cons 0 empty)))]
        [(symbol=? colour 'green) (cons 0 (cons 255 (cons 0 empty)))]
        [(symbol=? colour 'blue) (cons 0 (cons 0 (cons 255 empty)))]
        [(symbol=? colour 'white) (cons 255 (cons 255 (cons 255 empty)))]
        [(symbol=? colour 'black) (cons 0 (cons 0 (cons 0 empty)))]
        [(symbol=? colour 'magenta) (cons 255 (cons 0 (cons 255 empty)))]
        [(symbol=? colour 'yellow) (cons 255 (cons 255 (cons 0 empty)))]
        [(symbol=? colour 'cyan) (cons 0 (cons 255 (cons 255 empty)))]
        [else (cons -1 (cons -1 (cons -1 empty)))]))


;; Tests:

(check-expect(name->RGB 'white) (cons 255 (cons 255 (cons 255 empty))))
(check-expect(name->RGB 'red) (cons 255 (cons 0 (cons 0 empty))))
(check-expect(name->RGB 'green) (cons 0 (cons 255 (cons 0 empty))))
(check-expect(name->RGB 'blue) (cons 0 (cons 0 (cons 255 empty))))
(check-expect(name->RGB 'black) (cons 0 (cons 0 (cons 0 empty))))
(check-expect(name->RGB 'yellow) (cons 255 (cons 255 (cons 0 empty))))
(check-expect(name->RGB 'magenta) (cons 255 (cons 0 (cons 255 empty))))
(check-expect(name->RGB 'cyan) (cons 0 (cons 255 (cons 255 empty))))
(check-expect(name->RGB 'unknown) (cons -1 (cons -1 (cons -1 empty))))


;;Question 2c
;;(RGB->luminosity list-of-ints) produces the luminosity of the colour
;;Examples
(check-expect(RGB->luminosity (cons 0 (cons 0 (cons 0 empty)))) 0)
(check-expect(RGB->luminosity (cons 0 (cons 0 (cons 100 empty)))) 11)
(check-expect(RGB->luminosity (cons 0 (cons 100 (cons 0 empty)))) 59)

;; RGB->luminosity: RGB -> Num

(define (RGB->luminosity list-of-ints)
  (+ (* 0.3 (first list-of-ints))
     (* 0.59 (first (rest list-of-ints)))
     (* 0.11 (first (rest (rest list-of-ints))))))

;; Tests:

(check-expect(RGB->luminosity (cons 0 (cons 0 (cons 0 empty)))) 0)
(check-expect(RGB->luminosity (cons 0 (cons 0 (cons 100 empty)))) 11)
(check-expect(RGB->luminosity (cons 0 (cons 100 (cons 0 empty)))) 59)
(check-expect(RGB->luminosity (cons 100 (cons 0 (cons 0 empty)))) 30)
(check-expect(RGB->luminosity (cons 100 (cons 100 (cons 100 empty)))) 100)


;;Question 2d
;;(valid-RGB? list-of-vals) produces a boolean depending on whether its a valid RGB input

;;Examples
(check-expect (valid-RGB? 'red) false)
(check-expect (valid-RGB? (cons 255 (cons 0 (cons 0 empty)))) true)
(check-expect (valid-RGB? (cons 314 (cons 159 (cons 26 empty))))
false)
(check-expect (valid-RGB? "CMYK is the superior colour model") false)
;;valid-RGB?: (listof Any) -> Bool

(define (valid-RGB? list-of-vals)

(and (list? list-of-vals) (integer? (first list-of-vals))
    (<= (first list-of-vals) 255)
    (>= (first list-of-vals) 0)
    (integer? (first (rest (rest list-of-vals))))
    (<= (first (rest list-of-vals)) 255) 
    (>= (first (rest list-of-vals)) 0)
    (integer? (first (rest (rest list-of-vals))))
    (<= (first (rest (rest list-of-vals))) 255)
    (>= (first (rest (rest list-of-vals))) 0)
    (empty? (rest (rest (rest list-of-vals)
    )))))

;; Tests:

(check-expect (valid-RGB? 'green) false)
(check-expect (valid-RGB? (cons 100 (cons 0 (cons 0 empty)))) true)
(check-expect (valid-RGB? (cons 344 (cons 155 (cons 126 empty)))) false)
(check-expect (valid-RGB? 5) false)
(check-expect (valid-RGB? 'redandgreenandblue) false)

;;Question 2e

;;(Hex-letters num) if input is >10 this function converts it to Hex string otherwise
;;converts the number to a string

;;Examples
(check-expect (Hex-letters 6) "6")
(check-expect (Hex-letters 10) "A")

;;Hex-letters: Num -> String

(define (Hex-letters num)
  (cond [(< num 10) (number->string num)]
        [(= num 10) "A"]
        [(= num 11) "B"]
        [(= num 12) "C"]
        [(= num 13) "D"]
        [(= num 14) "E"]
        [(= num 15) "F"]))

;;Tests
(check-expect (Hex-letters 5) "5")
(check-expect (Hex-letters 10) "A")
(check-expect (Hex-letters 11) "B")
(check-expect (Hex-letters 12) "C")
(check-expect (Hex-letters 13) "D")
(check-expect (Hex-letters 14) "E")
(check-expect (Hex-letters 15) "F")


;;(RGB->hex rgb-list) produces a (listof Strings) comprising of the hex values of the RGB input

;;Examples
(check-expect (RGB->hex (cons 16 (cons 255 (cons 0 empty))))
              (cons "1" (cons "0" (cons "F" (cons "F" (cons"0" (cons "0" empty)))))))
(check-expect (RGB->hex (cons 16 (cons 80 (cons 0 empty))))
              (cons "1" (cons "0" (cons "5" (cons "0" (cons"0" (cons "0" empty)))))))

;;RGB->hex: RGB -> (list String String String
;;String String String)
(define (RGB->hex rgb-list)
  (cons (Hex-letters
         (quotient (first rgb-list) 16))
  (cons (Hex-letters
         (remainder (first rgb-list) 16))
  (cons (Hex-letters
         (quotient (first (rest rgb-list)) 16))
  (cons (Hex-letters
         (remainder (first (rest rgb-list)) 16))
  (cons (Hex-letters
         (quotient (first (rest (rest rgb-list))) 16))
  (cons (Hex-letters
         (remainder (first (rest (rest rgb-list))) 16))
           empty)))))))


;; Tests:
(check-expect (RGB->hex (cons 255 (cons 0 (cons 0 empty))))
              (cons "F" (cons "F" (cons "0" (cons "0" (cons "0" (cons "0" empty)))))))
(check-expect (RGB->hex (cons 255 (cons 255 (cons 0 empty))))
              (cons "F" (cons "F" (cons "F" (cons "F" (cons "0" (cons "0" empty)))))))
(check-expect (RGB->hex (cons 32 (cons 160 (cons 65 empty))))
              (cons "2" (cons "0" (cons "A" (cons "0" (cons "4" (cons "1" empty)))))))