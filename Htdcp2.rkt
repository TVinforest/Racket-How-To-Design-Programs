;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Htdcp2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe) 
(require 2htdp/image)

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


;;test ff
(define (ff a)
  (* 10 a))

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
  (cond
    [(>= 0]

