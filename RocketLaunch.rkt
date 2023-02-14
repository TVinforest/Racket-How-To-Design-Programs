;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname RocketLaunch) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
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
(define (PAWN ws)
  (overlay
   (circle (circle-state ws)  "solid" "green")
   (circle (/ WIDTH 50) "solid" "red")))

;;pawn center
(define CENTER (/ WIDTH 50))


;;circle-responce to launch
(define (circle-state ws)
   (cond
      ((string? ws) (/ WIDTH 50))
      ((<= ws 0) (abs ws))
      (else (modulo ws (/ WIDTH 50)))))

;;pawn speed
(define PAWN-SPEED 7)

;;delay befoer launch
(define DELAY 50)

;;render moving pawn and status
 (define (render ws)
   (place-image (PAWN ws) (- WIDTH (/ WIDTH 10)) (render-items ws) 
                 BACKGROUND))
 



;;new-position 
(define (state-loop ws)
  (cond
    ((string? ws) (new-state ws))
    ((< ws 0) (+ 1 ws))
    (else (+ ws PAWN-SPEED))))

;to draw itemization
(define (render-items ws)
    (cond
      ((string? ws) (render-class ws))
      ((<= ws 0) (- HEIGHT CENTER))
      (else (- (- HEIGHT CENTER) ws))))

;;state
(define (new-state ws)
    (cond
      ((string=? ws "resting") "resting")
      (else 200)))


;;reder class by string value
(define (render-class ws)
    (cond
      ((string=? ws "resting") (- HEIGHT CENTER))
      (else 200)))


;;launch test
(define (launch-test ws ke)
 (cond
  ((string=? ke "up") (- DELAY))
  ((string=? ke "down") "resting")
  (else ws)))

;;main  
(define (main ws)
  (big-bang ws
    (on-tick state-loop)
    (to-draw render)
    (on-key launch-test)
    ))

