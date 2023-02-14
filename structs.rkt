;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname structs) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe) 
(require 2htdp/image)
(require 2htdp/batch-io)


;;background measurements
(define WIDTH 800)
(define HEIGHT 800)

;;backgroud to draw on
(define BACKGROUND
    (rectangle WIDTH HEIGHT "solid" "black"))

;;controlled pawn
(define PAWN
    (circle (/ WIDTH 50) "solid" "red"))

;;pawn center
(define CENTER (/ WIDTH 50))

;;speed
(define SPEED 5)


;;velocity
(define-struct velocity (delta_x delta_y))

;;location


;;ball
(define-struct ball (location velocity))

;;
(define ball_0
  (make-ball (make-posn 50 50) (make-velocity -10 5)))

;;define dot
(define dot
  (make-posn 10 10))

;;define posn update
(define (posn-up-x p n)
  (make-posn (posn-x p) n))
  

;;main
(define (main ws)
  (big-bang ws
      (on-tick x+)
      (on-mouse reset-dot)         
      (to-draw scene+dot)))

;;on-tick update posn struct
(define (x+ ws)
  (posn-up-x ws (+ (posn-y ws) SPEED)))
  

;;reset dot position after rmb click
(define (reset-dot ws x y me)
  (cond
    ((mouse=? me "button-down") (make-posn x y))
    (else ws)))

;;draw dot and scene
(define (scene+dot ws)
  (place-image
   PAWN (posn-x ws) (posn-y ws) BACKGROUND))


;;UFO

;;ufo
(define-struct ufo (location velocity))

(define-struct location
  (make-posn x y))

;;define opsition update from velocity
(define (posn+ location velocity)
 (make-posn
  (+ (posn-x location) (velocity-delta_x velocity))
  (+ (posn-y location) (velocity-delta_y velocity))))

;;update ufo location
(define (ufo-move-1 ufo)
 (make-ufo (posn+ (ufo-location ufo) (ufo-velocity ufo))
  (ufo-velocity ufo)))

;;set velocity based on next goal location
(define (set-goal ufo x y me)
  (cond
   ((mouse=? me "button-down") (make-ufo (ufo-location ufo) (new-velocity ufo x y)))
   (else ufo)))


;;compute velocity based on the new goal
(define (new-velocity ufo x y)
  (make-velocity (/ (posn-x (relative-coord (ufo-location ufo) x y)) 50) 
                 (/ (posn-y (relative-coord (ufo-location ufo) x y)) 50)))


;;goal coordinates relative to ufo
(define (relative-coord location x y)
  (make-posn (- x (posn-x location)) (- y (posn-y location))))


;;draw canvas and ufo
(define (render-ufo ufo)
  (place-image
   PAWN (posn-x (ufo-location ufo)) (posn-y (ufo-location ufo)) BACKGROUND))



;;main ufo
(define (main_1 ufo)
  (big-bang ufo
    (on-tick ufo-move-1)
    (on-mouse set-goal)
    (to-draw render-ufo)))
  


;;define ufo instance

;initial location
(define location_0
  (make-posn 0 0))

;initial velocity
(define velocity_0
  (make-velocity SPEED SPEED))

;ufo instance
(define ufo_0
  (make-ufo location_0 velocity_0))


;;Time
(define-struct time-clock (h m s))

;;time to seconds
(define (time-to-seconds time-clock)
  (+ (* (time-clock-h time-clock) 60 60) (* (time-clock-m time-clock) 60) (time-clock-s time-clock)))

(define CT (make-time-clock 12 34 56))
   
(define-struct dog (Index Name))
;; (make-dog 1 "2" ))
 
;;distance 2d 
(define (distance-2d x y)
  (sqrt (+ (sqr x) (sqr y))))

;; word
(define-struct 3word (a b c))


;;compare letter equality
(define (compare-letter a b)
  (cond
    ((equal? a b) a)
    (else "#")))

;;compare 3letter word struct
(define (compare-words word0 word1)
  (string-append (compare-letter (3word-a word0) (3word-a word1))
                 (compare-letter (3word-b word0) (3word-b word1))
                 (compare-letter (3word-c word0) (3word-c word1))))


(define W1 (make-3word "r" "a" "b"))
(define W2 (make-3word "c" "a" "t"))




(define-struct game (ufo b))  


























