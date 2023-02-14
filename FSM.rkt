;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname FSM) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;FSM

(require 2htdp/universe) 
(require 2htdp/image)
(require 2htdp/batch-io)





;;background measurements
(define WIDTH 900)
(define HEIGHT 900)

;;backgroud to draw on
(define BACKGROUND
  (empty-scene WIDTH HEIGHT "black"))

;;countdown time
(define COUNTDOWN 3)


;;fsm state struct 
(define-struct sm-state (name stage))

;;state instance initial
(define ini-state (make-sm-state "red" 0))


;bang main
(define (main sm-state)
  (big-bang sm-state
     (on-tick tock 1)
    (to-draw render-fsm)
    (on-key start-fsm)))

;;each tick handle fsm state transition logic
(define (tock sm-state)
  (cond
    ((equal? (sm-state-name sm-state) "red") sm-state)
    ((equal? (sm-state-name sm-state) "green") (if (= (sm-state-stage sm-state) 0) (make-sm-state "countdown" COUNTDOWN) (make-sm-state "green" (- (sm-state-stage sm-state) 1))))
    ((equal? (sm-state-name sm-state) "countdown") (if (= (sm-state-stage sm-state) 1) (make-sm-state "red" 0) (make-sm-state "countdown" (- (sm-state-stage sm-state) 1))))
    (else (error "sm-state unspecified"))))

;;start fsm cycle pressing spacebar
(define (start-fsm sm-state ke)
 (cond
  ((and (equal? ke "up") (equal? (sm-state-name sm-state) "red")) (make-sm-state "green" COUNTDOWN))
  (else sm-state)))
  
;;render fsm
(define (render-fsm sm-state)
  (cond
    ((equal? (sm-state-name sm-state) "red") (place-image (rectangle 20 20 "solid" (sm-state-name sm-state)) (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND))
    ((equal? (sm-state-name sm-state) "green") (place-image (text (number->string (sm-state-stage sm-state)) 34 "green") (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND))
    ((equal? (sm-state-name sm-state) "countdown") (place-image (text (number->string (sm-state-stage sm-state)) 34  (if (odd? (sm-state-stage sm-state)) "red"  "yellow")) (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND) )
    (else (error "sm-state unspecified"))))
 

;;regex main
(define (fsm state)
  (big-bang state
    (to-draw render-regex)
    (on-key input-regex)))


(define (input-regex state ke)
  (cond
   ((equal? state "AA") (AA-trainsiton ke)) 
   ((equal? state "BB") (BB-trainsiton ke))
   ((equal? state "DD") "DD")
   ((equal? state "ER") "ER")))                          


;;state AA rules of transition
(define (AA-trainsiton ke)
  (cond
    ((equal? ke "a") "BB")
    (else "ER")))

;;state BB rules of transition
(define (BB-trainsiton ke)
  (cond
    ((or (equal? ke "b")(equal? ke "c")) "BB")
    ((equal? ke "d") "DD")
    (else "ER")))

;;render current state of regex fsm
(define (render-regex state)
  (place-image (rectangle WIDTH HEIGHT "solid" (state-to-color  state))  (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND))


;;choose color based on the current stated
(define (state-to-color  state)
  (cond
   ((equal? state "AA") "white") 
   ((equal? state "BB") "yellow")
   ((equal? state "DD") "green")
   ((equal? state "ER") "red")))
    