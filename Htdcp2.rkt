;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Htdcp2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/universe) 
(require 2htdp/image)
(require 2htdp/batch-io)


(define (y x) (* x x))


;;rocket animation constants

;;velocity constant
(define V 4.5)
;;distance formula
(define (distance t)
  (* V t))

;;canvas
(define HEIGHT 600)
(define WIDTH 800)
(define MTSCN (empty-scene WIDTH HEIGHT))

(define CENTER
  (/ WIDTH 2))

;;background image
(define BACKGROUND
  (rectangle WIDTH HEIGHT "solid" "blue"))


;;draw background
(define (draw-background h)
(place-image BACKGROUND CENTER (/ HEIGHT 2) MTSCN))

;;image to place
(define ROCKET
  (overlay (circle 10 "solid" "green")
         (rectangle 40 4 "solid" "green")))

(define ROCKET-CENTER-TO-TOP
  (- HEIGHT (/ (image-height ROCKET) 2)))

;;restrict image lowest position by it's height so it will never fall down trough canvas

(define (picture-of-rocket t)
  (cond
    [(<= (distance t) ROCKET-CENTER-TO-TOP)
     (place-image ROCKET CENTER (distance t)
                  MTSCN)]
    [(> (distance t) ROCKET-CENTER-TO-TOP)
     (place-image  ROCKET CENTER ROCKET-CENTER-TO-TOP
                  MTSCN)]))
;;image size
(define (image-size image)
  (* (image-width image) (image-height image)))




;;distance formula
(define (distance-from-origin x y)
(sqrt (+(sqr y)(sqr x))))


;;STRINGS

;;prefix suffix
(define prefix "hello")
(define suffix "world")

(define string-test "helloworld")
(define i 5)

;;insert string at i position of another string
(define (insert-string str stri i )
  (string-append (substring str 0 i) stri (substring str i)))

;;remove character fomr i position of a string
(define (remove-1string str i )
  (string-append (substring str 0 (- i 1)) (substring str i)))


;;get first 1string from non empty string 
(define (first-1string str)
  (cond
    [(equal? str "") ""]
    [else (substring str 1)]))


;;get last 1string from non empty string 
(define (last-1string str)
  (cond
    [(equal? str "") ""]
    [else (substring str (- (string-length str) 1))]))



;;BOOLEAN
(define sunny #true)
(define friday #false)

(define (==> sunny friday)
  (if (or (not sunny) friday) true false))


;;predicates testing
;;(define in ...)


;;to abs value
(define (abs-val n)
  (cond
    [(>= n 0) n]
    [else (* n -1)]))
 
;;apply based on type
(define (type-in in)
  (cond
    [(number? in) (abs-val in)]
    [(string? in) (string-length in)]
    [(image? in)(image-size in)]
    [(boolean? in) (if (equal? in #true) 10 20 )]))



;;
(define x 2)
(define (inverse-of-x x)
  (if (= x 0) 0 (/ 1 x)))
   

(define (f x) 1)


;;pythagorean distance 2d
(define (distance2d x y)
  (sqrt (+ (sqr x) (sqr y))))

;; raise x to the power of y
(define (x-to-power x y)
  (cond
    [( = y 0) 1]
    [#true (* x (x-to-power x ( - y 1)))]))
   

;;volume of a cube
(define (cvolume side)
  (x-to-power side 3))

;;surface of a cube
(define (csurface side)
   (* 6 (x-to-power side 2)))




;;image classify by w d ratio
(define (image-class image)
  (cond
    [(= (image-width image) (image-height image)) "square"]
    [(< (image-width image) (image-height image)) "tall"]
    [(> (image-width image) (image-height image)) "wide"]))


;;insert "_" between strings
(define (insert_ str i )
  (string-append (substring str 0 i) "_" (substring str i)))


;;remove string at i position of another string
(define (exclude-1string str i)
  (if (and (>= i 0) (= (- (string-length str) 1) i))
  (string-append (substring str 0 (- i 1)) (substring str i))
  ""))


;;letter opening
(define (opening first-name last-name)
  (string-append "Dear, " first-name))

;;test ff
(define (ff a)
  (* 10 a))


;;akermann function
(define (Akermann x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (Akermann (- x 1) (Akermann x (- y 1))))))

;;LETTER-BATCH

(define (letter first-name last-name signature)
  (string-append
   (beginning first-name)
   "\n\n"
   (body first-name last-name)
   "\n"
   (closure signature)))

(define (beginning first-name)
  (string-append "Dear " first-name ", "))

(define (body first-name last-name)
  (string-append
   "We have discovered that all people with the" "\n" 
   "last name " last-name " have won our lottery. So, " "\n"
   first-name ", " "hurry and pick up your prize."))

(define (closure signature-name)
  (string-append
   "\n\n"
   "Sincerely,"
   "\n\n"
   signature-name
   "\n"))

;;BEST PRICE FOR MOVIE

;;constant base cost to run a show
(define CONST-COST 180.0)

;;capita grow per price step
(define CAPITA-GROW 15.0)

;;price step
(define PRICE-STEP 0.1)

;;base attendance
(define BASE-ATTENDANCE 120.0)

;;base price
(define BASE-PRICE 5.0)

;;base cost of show for one attendee
(define BASE-COST 0.04)


;;change in attendance per step in price
(define CAPITA-PER-STEP (/ CAPITA-GROW PRICE-STEP))


;;attendies number based on ticket price
(define (attendees ticket-price)
  (round
  (- 120 (* (- ticket-price 5.0) CAPITA-PER-STEP))))

;;revenue of the movie theatre
(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

;;cost per show
(define (cost ticket-price)
  (+ 180 (* BASE-COST (attendees ticket-price))))

;;profit per show
(define (profit ticket-price)
  (cond
    ((<= (attendees ticket-price) 0) (error-message "at this price there will be no attendees"))
    (else (- (revenue ticket-price) (cost ticket-price)))))

;;movies data per show based on ticket cost
(define (movie-show-data ticket-price)
(error-message  (string-append
                 "profit: " (number->string (exact->inexact (profit ticket-price))) "\n"
                 "attendees: " (number->string (exact->inexact (attendees ticket-price))) "\n"
                 "cost: " (number->string (exact->inexact (cost ticket-price))) "\n"
                 "revenue: " (number->string (exact->inexact (revenue ticket-price))))))

;;ERROR-MESSAGE
(define (error-message string) 
(write-file 'stdout (string-append "\n" string " ")))




;;CONVERSION

;;farengheit to celcius 
(define (f2c f)
  (* 5/9 (- f 32)))

;;celcius to farengheit
(define (c2f c)
  (+(* 9/5  c) 32))

;;Convert values sepecified in one file into diff format and write to out file
(define (convert-in-out in out)
  (write-file out
    (string-append
     (number->string
      (f2c
       (string->number
        (read-file in))))
     "\n")))

;;fill letter template with strings from files
(define (main-letter in-fst in-lst in-sign out)
  (write-file out
              (letter
               (read-file in-fst)
               (read-file in-lst)
               (read-file in-sign))))





;;BIG BANG
(define (number->square s)
  (square s "solid" "red"))

(define (reset s ke)
  (* 2 s))

;;place a dot
(define BACKGROUND-0 (empty-scene 100 100))
(define DOT (circle 3 "solid" "red" ))

(define (main y)
  (big-bang y
    [on-tick sub1]
    [stop-when zero?]
    [to-draw place-dot-at]
    [on-key stop]))

(define (place-dot-at y)
  (place-image DOT 50 y BACKGROUND-0))

(define (stop y ke)
  0)

;;CHECKS
(check-expect (f2c -40) -40)
(check-expect (f2c 32) 0)
(check-expect (f2c 212) 100)


