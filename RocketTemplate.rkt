;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname RocketTemplate) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
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

;;pawn speed
(define PAWN-SPEED 7)

;;delay
(define DELAY 190)

;;main
(define (main ws)
  (big-bang ws
    (on-tick update-loop)
    (to-draw show)
    (on-key launch)))

;;update rocket state loop
(define (update-loop ws)
 (cond
   ((string? ws) "resting")
   (else (+ PAWN-SPEED ws))))
                
;;new-state-class
(define (new-state-class ws)
 (cond
   ((string=? ws "up") (- DELAY))
   ((string=? ws "resting") "resting")
   ))

;;render rocket
(define (show ws)
  (place-image (text (tostring ws) 24 "orange") (/ HEIGHT 2) (/ WIDTH 2)
  (place-image PAWN (/ WIDTH 2) (render-position ws) BACKGROUND)))

;;text filler coinvrsion
(define (tostring ws)
 (cond
  ((string? ws) ws)
  (else (number->string ws))))

;;render position
(define (render-position ws)
  (cond
    ((number? ws) (if ( >= ws 0) ( - (- HEIGHT CENTER) ws) (- HEIGHT CENTER)))
    (else (- HEIGHT CENTER))))
     
;;launch with pressing space
(define (launch ws ke)
 (cond
  ((string=? ke "up") (- DELAY))
  ((not (string=? ke "up")) "resting")))
