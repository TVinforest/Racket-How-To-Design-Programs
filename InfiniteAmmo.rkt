;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname InfiniteAmmo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe) 
(require 2htdp/image)
(require 2htdp/batch-io)



;;background measurements
(define WIDTH 1920)
(define HEIGHT 1000)

;;backgroud to draw on
(define BACKGROUND
    (rectangle WIDTH HEIGHT "solid" "black"))


;;shot
(define SHOT (triangle 10 "solid" "red"))

;;center
(define XSHOTS (/ WIDTH 2))

; ShotWorld -> ShotWorld 
(define (main w)
  (big-bang w
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))
 
; ShotWorld -> ShotWorld 
; moves each shot up by one pixel 
(define (tock w)
  (cond
    ((empty? w) '())
    (else (cond
            ((not (positive? (car w))) (tock (rest w))) 
            (else (cons (- (first w) 10) (tock (rest w))))))))
 
; ShotWorld KeyEvent -> ShotWorld 
; adds a shot to the world if the space bar is hit 
(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))
 
; ShotWorld -> Image 
; adds each shot y on w at (XSHOTS,y} to BACKGROUND
(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT XSHOTS (first w)
                       (to-image (rest w)))]))




(define list_0 '(2 1 4 1 4 6 7 77 88 12 22))


  
;;remove all entries matching the rule
(define (remove-element list test)
  (cond
    ((empty? list) '())
    ((test (car list)) (remove-element (cdr list) test))
    (else (cons (car list) (remove-element (cdr list) test)))))


                       