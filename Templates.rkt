;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Templates) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (... ...)
 (cond
   ((...) (...))
   ((...) (...))))


(define (si-render s)
  (cond
    ((aim? s) (... (aim-tank s) ... (aim-ufo s) ...))
    ((fired? s) (... (fired-tank s) ... (fired-ufo s)
                 ... (fired-missile s) ...))))