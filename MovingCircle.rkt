;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname MovingCircle) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe) 
(require 2htdp/image)
(require 2htdp/batch-io)




;;background measurements
(define WIDTH 900)
(define HEIGHT 800)

;;backgroud to draw on
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "black" ))


;;pawn struct position and direction
(define-struct pawn (position direction))

;;pawn inst
(define default_pawn (make-pawn (/ WIDTH 2) 1))


;;pawn initial direction
(define INITIAL-DIRECTION -)

;;controlled pawn
(define PAWN
  (circle (/ WIDTH 50) "solid" "red"))

;;pawn speed
(define PAWN-SPEED 7)
  
;;intervals for pawn status
 (define  LANDING-MARGIN (/ HEIGHT 3))
 (define CLOSE ( + (/ (image-width PAWN) 2) LANDING-MARGIN))
 (define HIGH (* (image-width PAWN) 30))   

;;status line font size
(define FONT-SIZE 20)
;;status line
(define (STATUS-LINE pawn)
  (cond
    ((>= (pawn-position pawn) CLOSE)
     (text (string-append (number->string ( - (pawn-position pawn) (/ (image-width PAWN) 2))) " high altitude " (number->string ( - (pawn-direction pawn)))) FONT-SIZE "green"))
    ((< WIDTH-LIMIT-L (pawn-position pawn) CLOSE)
     (text (string-append (number->string ( - (pawn-position pawn) (/ (image-width PAWN) 2))) " approaching ground " (number->string ( - (pawn-direction pawn)))) FONT-SIZE "yellow"))
    ((<= (pawn-position pawn) WIDTH-LIMIT-L)
     (text (string-append (number->string ( - (pawn-position pawn) (/ (image-width PAWN) 2))) " land "(number->string ( - (pawn-direction pawn)))) FONT-SIZE "red"))))


;;affect positiona
(define (effect pawn ke)
  (cond
    ((string=? "right" ke) (make-pawn (movement-limits (+ (pawn-position pawn) (* PAWN-SPEED 2))) (pawn-direction pawn)))
    ((string=? "left" ke) (make-pawn (movement-limits (- (pawn-position pawn) (* PAWN-SPEED 2)))(pawn-direction pawn)))
     (else (make-pawn (movement-limits (- (pawn-position pawn) PAWN-SPEED ))(pawn-direction pawn))) ))

;;position limits

;;width limits
(define WIDTH-LIMIT-R (- WIDTH (/ (image-width PAWN) 2)))
(define WIDTH-LIMIT-L (+ 100 (/ (image-width PAWN) 2)))


(define (movement-limits pawn-position)
  (cond
    ((> pawn-position WIDTH-LIMIT-R) WIDTH-LIMIT-R)
    ((< pawn-position WIDTH-LIMIT-L) WIDTH-LIMIT-L)
    (else pawn-position)))


;;render moving pawn and status
 (define (render pawn)
   (place-image
    (STATUS-LINE pawn) (/ WIDTH 2) (- HEIGHT (/ HEIGHT 20)) 
    (place-image PAWN (pawn-position pawn)  (- HEIGHT(/ HEIGHT 10))
                 BACKGROUND)))

;;constantly move pawn in opposite direction from last visited border of frame
(define (update pawn)
  (make-pawn ( + (pawn-position pawn) (* PAWN-SPEED (direction-operator? pawn))) (direction-operator? pawn)))

(define (direction-operator? circle)
  (cond
    ((> (pawn-position circle) WIDTH-LIMIT-R) (* (pawn-direction circle) -1))
    ((< (pawn-position circle) WIDTH-LIMIT-L) (* (pawn-direction circle) -1))
    (else (pawn-direction circle))))


;;main
(define (main pawn)
  (big-bang pawn
    (on-tick update)
    (to-draw render)
    (on-key effect)
    ))