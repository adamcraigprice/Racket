;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname examples-a06) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; A06 Examples
;;

;; Question 1a

(check-expect (my-list-ref (list 1 2 3 4) 0) 1)
(check-expect (my-list-ref (list 5 4 3) 2) 3)
(check-expect (my-list-ref (list 2) 20) false)
(check-expect (my-list-ref (list 5.6 7 -3.2 4 0 9) 4) 0)
(check-expect (my-list-ref (list 1) 1) false)

;; Question 1b

(check-expect (zip (list 1 2 3 4) (list "a" "b" "c" "d"))
(list (list 1 "a") (list 2 "b") (list 3 "c") (list 4 "d")))
(check-expect (zip empty empty) empty)
(check-expect (zip (0 1) (list "panda" "elephant")) (list (list 0 "panda") (list 1 "elephant")))
(check-expect (zip (4 5 6) ("red" "blue" "yellow"))
              (list (list 4 "red") (list 5 "blue") (list 6 "yellow")))
(check-expect (zip (list 17) (list "potato")) (list (list 17 "potato")))

;; Question 1c

(check-expect (list-xor (list 1 3 5) (list 2 3 4)) (list 1 2 4 5))
(check-expect (list-xor (list 0 1) (list 0 1)) (list))
(check-expect (list-xor (list 0 1) (list 0 1 2)) (list 2))
(check-expect (list-xor (list 7 10 12 14 500) (list 3 10 14 65 500 520)) (list 3 7 12 65 520))
(check-expect (list-xor (list -12 24.5) (list 3 17)) (list -12 3 17 24.5))

;; Question 2c

(check-expect (num->signmag 0) (make-signmag 'zero 1))
(check-expect (num->signmag -12) (make-signmag 'negative 12))
(check-expect (num->signmag 12) (make-signmag 'positive 12))
(check-expect (num->signmag 0.615) (make-signmag 'positive 0.615))
(check-expect (num->signmag -0.615) (make-signmag 'negative 0.615))

;; Question 2d

(check-expect (signmag->num (make-signmag 'zero 1)) 0)
(check-expect (signmag->num (make-signmag 'negative 12)) -12)
(check-expect (signmag->num (make-signmag 'positive 12)) 12)
(check-expect (signmag->num (make-signmag 'positive 0.615)) 0.615)
(check-expect (signmag->num (make-signmag 'negative 0.615)) -0.615)

;; Question 3a

(check-expect (full? empty) true)
(check-expect (full? (make-node 7 empty empty)) true)
(check-expect (full? (make-node 7 (make-node 4 empty empty) empty)) false)
(check-expect (full? (make-node 7 (make-node 4 empty empty) (make-node 9 empty empty))) true)
(check-expect (full? (make-node 7 (make-node 4 (make-node 2 empty empty) empty)
                                (make-node 9 empty empty))) false)

;; Question 4 Prerequisites

(define instr-actlst  
  (list (make-action "Zahra" -4 "Procrastinated on CS 135 assignment")
        (make-action "Armin" 10 "Helped Zahra proofread the assignment")
        (make-action "Mark" 3 "Shared his lego with Byron during playtime")
        (make-action "Patrick" -10 "Joined a bicycle gang")
        (make-action "Armin" -1 "Stayed up too late on a schoolnight")
        (make-action "Charlie" -5 "Scribbled on the walls")
        (make-action "Byron" -4 "Harassed a goose in the park")
        (make-action "Zahra" -5 "Ate the ISAs' Cheetos")
        (make-action "Armin" 6 "Did his homework before going out to play")
        (make-action "Zahra" 3 "Cleaned her room without being nagged")
        (make-action "Zahra" -7 "Was disobedient at the grocery store")
        (make-action "Charlie" 5 "Saved Patrick from a rival bicycle gang")))
(define instr-childrenlist 
  (list (list "Byron"   (list (make-wish 52 "Commodore 64")
                              (make-wish 17 "Hot Wheels")
                              (make-wish 10 "Colourful Pencils")))
        
        (list "Zahra"   (list (make-wish 57 "Ken's Mojo Dojo Casa House")
                              (make-wish 43 "Bibble Plushie")))
        
        (list "Mark"    (list (make-wish 37 "Telescope")
                              (make-wish 11 "Lego flowers")))
        
        (list "Charlie" (list (make-wish 32 "Amigurumi Bee Plushie")
                              (make-wish 20 "Chemistry Set")))
        
        (list "Patrick" (list (make-wish 42 "Nerf gun")
                              (make-wish 20 "Elvis Head Chia Pet")))
        
        (list "Armin"   (list (make-wish 54 "Toy Car")
                              (make-wish 32 "Very big pizza")
                              (make-wish 15 "Ice Cream")))))

;; Question 4a

(check-expect (extreme-actions "Zahra" instr-actlst)
(list "Was disobedient at the grocery store" "Cleaned her room
without being nagged"))
(check-expect (extreme-actions "Patrick" instr-actlst)
(list "Joined a bicycle gang" "Joined a bicycle gang"))
(check-expect (extreme-actions "Sam" instr-actlst)(list))
(check-expect (extreme-actions "Charlie" instr-actlst)
(list "Scribbled on the walls" "Saved Patrick from a rival bicycle gang"))
(check-expect (extreme-actions "Armin" instr-actlst)
(list "Stayed up too late on a schoolnight" "Helped Zahra proofread the assignment"))

;; Question 4b

(check-expect (gifts-received "Byron" 20 instr-childrenlist -10)
(list "Playing Card Deck" "Colourful Pencils" "Hot Wheels"))
(check-expect (gifts-received "Armin" 10 instr-childrenlist -11) (list "Playing Card Deck"))
(check-expect (gifts-received "Patrick" -10 instr-childrenlist -12) 'coal)
(check-expect (gifts-received "Zahra" -15 instr-childrenlist -13) 'krampus)
(check-expect (gifts-received "Thomas" 100 instr-childrenlist -9) empty)

;; Question 4c

(check-expect (add-action (make-actionnode
   "Mark"
   12
   (list (make-action "Mark" 12 "Bought the ISAs more Cheetos"))
   (make-actionnode 
    "Byron"
    2
    (list (make-action "Byron" 2 "Recycled an empty Cheetos packet"))
    empty
    empty)
   (make-actionnode 
    "Zahra" 
    -5
    (list (make-action "Zahra" -5 "Ate the ISAs' Cheetos"))
    empty
    empty)) (make-action "Byron" -7 "Joined a gang"))
    (make-actionnode
   "Mark"
   12
   (list (make-action "Mark" 12 "Bought the ISAs more Cheetos"))
   (make-actionnode 
    "Byron"
    -5
    (list (make-action "Byron" 2 "Recycled an empty Cheetos packet")
          (make-action "Byron" -7 "Joined a gang"))
    empty
    empty)
   (make-actionnode 
    "Zahra" 
    -5
    (list (make-action "Zahra" -5 "Ate the ISAs' Cheetos"))
    empty
    empty)))
(check-expect (add-action (make-actionnode
   "Mark"
   12
   (list (make-action "Mark" 12 "Bought the ISAs more Cheetos"))
   (make-actionnode 
    "Byron"
    2
    (list (make-action "Byron" 2 "Recycled an empty Cheetos packet"))
    empty
    empty)
   (make-actionnode 
    "Zahra" 
    -5
    (list (make-action "Zahra" -5 "Ate the ISAs' Cheetos"))
    empty
    empty)) (make-action "Jennifer" 60 "Performed CPR on elderly women"))
    (make-actionnode
   "Mark"
   12
   (list (make-action "Mark" 12 "Bought the ISAs more Cheetos"))
   (make-actionnode 
    "Byron"
    2
    (list (make-action "Byron" 2 "Recycled an empty Cheetos packet"))
    empty
    (make-actionnode 
    "Jennifer"
    60
    (list (make-action "Jennifer" 60 "Performed CPR on elderly women"))
    empty
    empty))
   (make-actionnode 
    "Zahra" 
    -5
    (list (make-action "Zahra" -5 "Ate the ISAs' Cheetos"))
    empty
    empty)))

;; Question 4d

(check-expect
 (gift-list
  (make-actionnode
   "Mark"
   12
   (list (make-action "Mark" 12 "Bought the ISAs more Cheetos"))
   (make-actionnode 
    "Byron"
    2
    (list (make-action "Byron" 2 "Recycled an empty Cheetos packet"))
    empty
    empty)
   (make-actionnode 
    "Zahra" 
    -5
    (list (make-action "Zahra" -5 "Ate the ISAs' Cheetos"))
    empty
    empty)) instr-childrenlist -12)
 (list (list "Byron" (list "Playing Card Deck")) 
       (list "Mark" (list "Playing Card Deck" "Lego flowers"))
       (list "Zahra" 'coal)))
(check-expect
 (gift-list
  (make-actionnode
   "Mark"
   10
   (list (make-action "Mark" 10 "Bought the ISAs more Cheetos"))
   (make-actionnode 
    "Byron"
    20
    (list (make-action "Byron" 20 "Recycled an empty Cheetos packet"))
    empty
    empty)
   (make-actionnode 
    "Zahra" 
    -50
    (list (make-action "Zahra" -50 "Ate the ISAs' Cheetos"))
    empty
    empty)) instr-childrenlist -12)
 (list (list "Byron" (list "Playing Card Deck" "Hot Wheels" "Colourful Pencils")) 
       (list "Mark" (list "Playing Card Deck"))
       (list "Zahra" 'krampus)))
    