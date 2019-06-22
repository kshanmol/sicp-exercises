#lang sicp

(define (product term a next b)
  (cond ((> a b) 1)
        (else (* (term a) (product term (next a) next b)))))

(define (iter current term a next b)
  (cond ((> a b) current)
        (else (iter (* current (term a)) term (next a) next b))))

(define (product-iter term a next b)
  (iter 1 term a next b))

(define (factorial a)
  (define (increment a)
    (+ 1 a))
  (product-iter identity 1 increment a))

(define (pi x)
  (define (f n)
    (/ (* (- n 1) (+ n 1)) (* n n)))
  (define (incr n)
    (+ n 2))
  (* 4 (product-iter f 3 incr (* 2 (+ x 1)))))


