;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Tax) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe) 
(require 2htdp/image)
(require 2htdp/batch-io)



(define MEDIUM-PRICE 1000)
(define LUX-PRICE 10000)

(define (precents-of precents of)
  (* (/ of 100) precents))

(define (tax price)
  (cond
    ((< 0 price MEDIUM-PRICE) 0)
    ((<= 1000 price LUX-PRICE) (precents-of 5 price))
    ((>= price 10000) (precents-of 8 price))
    (else "wrong price")))

(define (n-steps n)
  (cond
    ((<= n 1) 1)
    (else (+ (n-steps ( - n 1)) (n-steps ( - n 2))))))


;;background measurements
(define WIDTH 800)
(define HEIGHT 800)

;;backgroud to draw on
(define BACKGROUND
    (rectangle WIDTH HEIGHT "solid" "black"))

;;door fsm
(define (main ws)
  (big-bang ws
      (on-tick close-door 3)
      (on-key update-door)
      (to-draw render-door)))

;;try to close the door each tick
(define (close-door ws)
  (cond
    ((equal? ws "open") "closed")
    (else ws)))

;;control the door
(define (update-door ws ke)
  (cond
    ((and (equal? ke " ") (equal? ws "closed")) "open")
    ((and (equal? ke "u") (equal? ws "locked")) "closed")
    ((and (equal? ke "l") (equal? ws "closed")) "locked")
    (else ws)))

;;render the door state
(define (render-door ws)
  (place-image 
   (text ws 20 "red") (/ WIDTH 2)  (/ HEIGHT 2) BACKGROUND))




;;distance 2d
(define (distance-to-0 ap)
 (sqrt
  (+ (sqr (posn-x ap))
     (sqr (posn-y ap)))))

;;distance Manhattan
(define (distance-Manhattan ap)
  (+ (posn-x ap)
     (posn-y ap)))

;;function composition 
(define (comp a b)
  (λ (x)
    (a (b x))))

;;function id 
(define (id a)
  a)

;;test if defined composition function preserves id laws
(define (test-comp comp a x)
 (and
  (equal? ((comp a id) x) (a x))
  (equal? ((comp id a) x) (a x))))
  



;;STRUCTURE

(define-struct hero (name age class))
(define-struct class (name level))


(define SORROW
  (make-hero "sorrow" 15 (make-class "wizard" 18)))

















