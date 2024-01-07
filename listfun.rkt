;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname listfun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;Question 2a
;;Examples
(check-expect (is-vowel? #\i) true)
(check-expect (is-vowel? #\h) false)

;;(is-vowel? vowel?-char) checks if a character is a vowel
;;is-vowel?: Char -> Bool
(define (is-vowel? vowel?-char)
  (or (char=? vowel?-char #\a)  
  (char=? vowel?-char #\i) 
  (char=? vowel?-char #\o) 
  (char=? vowel?-char #\e) 
  (char=? vowel?-char #\u)
  (char=? vowel?-char #\A)  
  (char=? vowel?-char #\I) 
  (char=? vowel?-char #\O) 
  (char=? vowel?-char #\E) 
  (char=? vowel?-char #\U)))

;;Tests
(check-expect (is-vowel? #\i) true)
(check-expect (is-vowel? #\A) true)
(check-expect (is-vowel? #\o) true)
(check-expect (is-vowel? #\i) true)
(check-expect (is-vowel? #\I) true)
(check-expect (is-vowel? #\h) false)

;;Examples
(check-expect (count-vowels-reccursion (cons #\i (cons #\0 empty))) 1)
(check-expect (count-vowels-reccursion (cons #\5 empty)) 0)
(check-expect (count-vowels-reccursion (cons #\q empty)) 0)

;;(count-vowels-reccursion char-list) counts the number of vowels in a list of characters
;;count-vowels-reccursion: listof Char -> Nat
(define (count-vowels-reccursion char-list)
  (cond [(empty? char-list) 0]
        [(is-vowel? (first char-list)) (+ 1 (count-vowels-reccursion (rest char-list)))]
        [else (count-vowels-reccursion (rest char-list))]))

;;Examples
(check-expect
(count-vowels "the quick brown fox jumped over the dog") 11)
(check-expect (count-vowels "hElLo") 2)
(check-expect (count-vowels "qwrtypsdfghjklzxcvbnm") 0)

;;(count-vowels vowel-string) counts the vowels in a string
;;count-vowels: String -> Nat
(define (count-vowels vowel-string)
  (count-vowels-reccursion (string->list vowel-string)))

;;Tests
(check-expect (count-vowels "the brown jumped over the dog") 8)
(check-expect (count-vowels "hlLo") 1)
(check-expect (count-vowels "qwrtypsdfghjjjjjjjjjjjjjklzxcvbnm") 0)

;;Question 2b


;;Examples
(check-expect (sorted? (cons "hello" (cons "Hello" empty))) false)
(check-expect (sorted? (cons "Hi" (cons "hellooo"
(cons "how are youu" empty)))) true)
(check-expect (sorted? (cons "hello" (cons "hello" (cons "hey"
(cons "hiiii" empty))))) false)

;;(sorted? list-of-strings) checks if a listof strings are sorted in lexographical order
;;sorted?: listof String -> Bool
(define (sorted? list-of-strings)
  (cond [(empty? (rest list-of-strings)) true]
        [(string<? (first list-of-strings)
         (first (rest list-of-strings)))
         (sorted? (rest list-of-strings))]
        [else false]))

;;Tests
(check-expect (sorted? (cons "heGGo" (cons "Hello" empty))) false)
(check-expect (sorted? (cons "Hi" (cons "hellooo"
(cons "ho" empty)))) true)
(check-expect (sorted? (cons "hello" (cons "hello" (cons "hey"
(cons "hiiifffi" empty))))) false)


;;Question 2c

;;Examples for 2c

(check-expect (replace-word "exam" "assessments"
(cons "content" (cons "exam" (cons "assignments"
empty))))
(cons "content" (cons "assessments" (cons "assignments" empty))))

(check-expect (replace-word "Bees" "I"
(cons "Bees" (cons "Hate" (cons "I"
empty))))
(cons "I" (cons "Hate" (cons "I" empty))))

;;(replace-word original-word new-word list-of-strings) replaces the original word
;; with the new word in a list of strings 
;;replace-word: String String listof Strings -> listof Strings

(define (replace-word original-word new-word list-of-strings)
  (cond [(empty? (rest list-of-strings)) list-of-strings]
        [(string=? (first list-of-strings) original-word)
         (cons new-word
(replace-word original-word new-word (rest list-of-strings)))]
        [else (cons (first list-of-strings)
(replace-word original-word new-word (rest list-of-strings)))]))

;;Tests
(check-expect (replace-word "exam" "assessments"
(cons "heyyyyyy" (cons "exam" (cons "assignments"
empty))))
(cons "heyyyyyy" (cons "assessments" (cons "assignments" empty))))

(check-expect (replace-word "Bees" "I"
(cons "Bees" (cons "He" (cons "y"
empty))))
(cons "I" (cons "He" (cons "y" empty))))

;;Question 2d

;;Examples

(check-expect (remove-duplicates (cons 1 (cons "I" (cons 1 (cons "I"
(cons "I" (cons 1 (cons 'hey (cons 1
(cons 'hey empty))))))))))
(cons "I" (cons 1 (cons 'hey empty))))

(check-expect (remove-duplicates (cons 1 (cons 3 (cons 1 (cons 2
(cons 4 (cons 2 (cons 7 (cons 2
(cons 2 empty))))))))))
(cons 3 (cons 1 (cons 4 (cons 7 (cons 2 empty))))))


;;(remove-duplicates list-of-any) removes the duplicates in a list
;;remove-duplicates: listof Any -> listof Any
(define (remove-duplicates list-of-any)
  (cond [(empty? list-of-any) list-of-any]
        [(member? (first list-of-any) (rest list-of-any))
         (remove-duplicates (rest list-of-any))]
        [else (cons (first list-of-any) (remove-duplicates (rest list-of-any)))]))

;;Tests
(check-expect (remove-duplicates (cons 1 (cons "I" (cons 1 (cons "I"
(cons 1 (cons 1 (cons 'hey (cons 1
(cons 'hey empty))))))))))
(cons "I" (cons 1 (cons 'hey empty))))

(check-expect (remove-duplicates (cons 1 (cons 1 (cons 3 (cons 1 (cons 2
(cons 4 (cons 2 (cons 7 (cons 2
(cons 2 empty)))))))))))
(cons 3 (cons 1 (cons 4 (cons 7 (cons 2 empty))))))

