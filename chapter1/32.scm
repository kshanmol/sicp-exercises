#lang sicp

(define (accumulate combiner null-value term a next b)
  (cond ((> a b) null-value)
        (else (combiner (term a) (accumulate combiner null-value term (next a) next b)))))

(define (filter-accumulate predicate combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((predicate a) (combiner (term a) (filter-accumulate predicate combiner null-value term (next a) next b)))
        (else (filter-accumulate predicate combiner null-value term (next a) next b))))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (factorial a)
  (define (increment a)
    (+ 1 a))
  (product identity 1 increment a))

(define (summation n)
  (define (increment a)
    (+ 1 a))
  (sum identity 1 increment n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n current)
  (cond ((> (* current current) n) n)
        ((divides? current n) current)
        (else (find-divisor n (next-divisor current)))))

(define (next-divisor n)
  (cond ((= n 2) 3)
        (else (+ n 2))))

(define (divides? divisor quotient)
  (= (remainder quotient divisor) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (prime-square-sum a b)
  (filter-accumulate prime? + 0 (lambda (x) (* x x)) a (lambda (x) (+ 1 x)) b))
