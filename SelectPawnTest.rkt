;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname SelectPawnTest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe) 
(require 2htdp/image)
(require 2htdp/batch-io)




;;background measurements
(define WIDTH 1920)
(define HEIGHT 1000)

;;backgroud to draw on
(define BACKGROUND
    (rectangle WIDTH HEIGHT "solid" "black"))

;;controlled pawn
(define PAWN
    (circle (/ WIDTH 50) "solid" "red"))

;;controlled pawn
(define PAWN_1
    (circle (/ WIDTH 50) "solid" "green"))

;;pawn center
(define CENTER (/ WIDTH 50))

;;speed
(define SPEED 5)

(define VELOCITY-TO-DISTANCE-RATIO 30)

;;movement limits
(define HEIGHT-LIMIT-U 0)

(define HEIGHT-LIMIT-D HEIGHT)

(define WIDTH-LIMIT-L 0)

(define WIDTH-LIMIT-R WIDTH)



;;UFO

;;ufo
(define-struct ufo (location velocity))

;;location
(define-struct location
  (make-posn x y))

;;velocity
(define-struct velocity (delta_x delta_y))

;;ufo group struct
(define-struct herd (ufo-0 ufo-1 selected-index))



;;define ufo instance 0

;initial location
(define location_0
  (make-posn 150 400))

;initial velocity
(define velocity_0
  (make-velocity 0 0))

;ufo instance
(define ufo_0
  (make-ufo location_0 velocity_0))

;;define ufo instance 1

;initial location
(define location_1
  (make-posn 50 50))

;initial velocity
(define velocity_1
  (make-velocity 0 0))

;ufo instance
(define ufo_1
  (make-ufo location_1 velocity_1))


;;herd struct instance
(define herd_0 (make-herd ufo_0 ufo_1 1))




;;main ufo
(define (main herd)
  (big-bang herd
    (check-with herd?) 
    (on-tick ufo-move)
    (on-mouse select-or-control)
    (to-draw render-ufo)))
 



;;update ufo location
(define (ufo-move herd)
 (make-herd (make-ufo (posn+ (ufo-location (herd-ufo-0 herd)) (ufo-velocity (herd-ufo-0 herd))) (ufo-velocity (herd-ufo-0 herd)))
            (make-ufo (posn+ (ufo-location (herd-ufo-1 herd)) (ufo-velocity (herd-ufo-1 herd))) (ufo-velocity (herd-ufo-1 herd)))
            (herd-selected-index herd)))

;;position update  (+ position velocity)
(define (posn+ location velocity)
 (make-posn
  (movement-limits-x (+ (posn-x location) (velocity-delta_x velocity)))
  (movement-limits-y (+ (posn-y location) (velocity-delta_y velocity)))))


;;limit pawn X movement by boundaries of a canvas
(define (movement-limits-x location-x)
  (cond
    ((> location-x (- WIDTH-LIMIT-R (/ (image-width PAWN) 2))) (- WIDTH-LIMIT-R (/ (image-width PAWN) 2)))
    ((< location-x (+ WIDTH-LIMIT-L (/ (image-width PAWN) 2))) (+ WIDTH-LIMIT-L (/ (image-width PAWN) 2)))
    (else location-x)))

;;limit pawn Y movement by boundaries of a canvas
(define (movement-limits-y location-y)
  (cond
    ((> location-y (- HEIGHT-LIMIT-D (/ (image-width PAWN) 2))) (- HEIGHT-LIMIT-D (/ (image-width PAWN) 2)))
    ((< location-y (+ HEIGHT-LIMIT-U (/ (image-width PAWN) 2))) (+ HEIGHT-LIMIT-U (/ (image-width PAWN) 2)))
    (else location-y)))






;;select pawn if mouse click was over it if not set new velocity based on x y mouse click coord for already selected pawn
(define (select-or-control herd x y me)
  (cond
    ((equal? me "button-down") (cond
                              ((click-on-cirlce? (herd-ufo-0 herd) PAWN x y) (make-herd (herd-ufo-0 herd) (herd-ufo-1 herd) 0))
                              ((click-on-cirlce? (herd-ufo-1 herd) PAWN x y) (make-herd (herd-ufo-0 herd) (herd-ufo-1 herd) 1))
                              (else (set-new-direction herd x y))))                                              
     (else herd)))

;set new goal for selected pawn
(define (set-new-direction herd x y)
  (cond
    ((equal?(herd-selected-index herd) 0) (make-herd (make-ufo (ufo-location (herd-ufo-0 herd)) (new-velocity (herd-ufo-0 herd) x y)) (herd-ufo-1 herd) 0))
    ((equal?(herd-selected-index herd) 1) (make-herd (herd-ufo-0 herd) (make-ufo (ufo-location (herd-ufo-1 herd)) (new-velocity (herd-ufo-1 herd) x y)) 1))))


;;compute velocity based on the new goal
(define (new-velocity ufo x y)
  (make-velocity (/ (posn-x (relative-coord (ufo-location ufo) x y)) VELOCITY-TO-DISTANCE-RATIO) 
                 (/ (posn-y (relative-coord (ufo-location ufo) x y)) VELOCITY-TO-DISTANCE-RATIO)))


;;goal coordinates relative to ufo
(define (relative-coord location x y)
  (make-posn (- x (posn-x location)) (- y (posn-y location))))



;;calculateif mouse click was in area covered by circle figure of pawn
(define (click-on-cirlce? ufo ufo-image x y)
  (and (x-in-range? ufo ufo-image x) (y-in-range? ufo ufo-image x y)))


;;is x position of a mouse click in range of circle width on x coord
(define (x-in-range? ufo ufo-image x)
  (if (< (- (posn-x (ufo-location ufo)) (/ (image-width ufo-image) 2)) x (+ (posn-x (ufo-location ufo)) (/ (image-width ufo-image) 2))) #true #false))

       
;;calculate y coord range that is on circle area based on the current x
(define (y-in-range? ufo ufo-image x y)
  (if (< (- (posn-y (ufo-location ufo)) (sqrt (- (sqr (/ (image-width ufo-image) 2)) (sqr (- (posn-x (ufo-location ufo)) x)))))
         y
         (+ (posn-y (ufo-location ufo)) (sqrt (- (sqr (/ (image-width ufo-image) 2)) (sqr (- (posn-x (ufo-location ufo)) x)))))) #true #false))
  





;;draw canvas and ufo
(define (render-ufo herd)
  (place-image
         (select-pawn-image 1 herd) (posn-x (ufo-location (herd-ufo-1 herd))) (posn-y (ufo-location (herd-ufo-1 herd)))
   (place-image
          (select-pawn-image 0 herd) (posn-x (ufo-location (herd-ufo-0 herd))) (posn-y (ufo-location (herd-ufo-0 herd))) BACKGROUND)))


;;select pawn to render
(define (select-pawn-image ufo-index herd)
  (cond
    ((equal? (herd-selected-index herd) ufo-index) PAWN_1)
    (else PAWN)))






















