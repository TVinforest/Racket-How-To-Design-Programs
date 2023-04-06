;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname RandomFromNet) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (sum-n n)
  (cond
    ((= n 0) 0)
    (else (+ (sum-n (sub1 n)) n))))

(define (path-n n m)
  (cond
    ((= n 0) 0)
    ((or (= n 1) (= m 1)) 1)
    (else (+ (path-n n (sub1 m)) (path-n (sub1 n) m)))))

(define (path-n2 n c m)
  (cond
    ((= c 0) (/ 1 (fact (sub1 m))))
    (else (* n (path-n2 n (sub1 c) m)))))

(define (path-n3 n m)
  (/ (power n (sub1 m)) (fact (sub1 m))))
   

(define (power n e)
 (cond
  ((= e 0) 1)
  (else (* n (power n (sub1 e))))))
     

(define (fact n)
  (cond
    ((= n 1) 1)
    (else (* n (fact (sub1 n))))))