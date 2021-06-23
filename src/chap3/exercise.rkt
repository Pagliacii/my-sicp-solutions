#!/usr/bin/env racket

#lang racket

(define the-empty-stream '())
(define stream-null? null?)
(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream head tail)
     (cons head (delay tail))]))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))
(define (show-stream s n)
  (define (iter i)
    (if (= i n)
        (display-line 'done)
        (begin (display-line (stream-ref s i))
               (iter (+ i 1)))))
  (iter 0))

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define (divisible? x y) (= (remainder x y) 0))
(define ones (cons-stream 1 ones))
(define integers
  (cons-stream 1 (add-streams ones integers)))
(define fibs
  (cons-stream
   0
   (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))
(define double (cons-stream 1 (scale-stream double 2)))
(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))
(define (square x) (* x x))
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(define (mul-streams s1 s2) (stream-map * s1 s2))
(define factorials
  (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

(define (partial-sums s)
  (add-streams s (cons-stream 0 (partial-sums s))))
(define (partial-stream->list s n)
  (define (rec str i)
    (if (= i n)
        '()
        (cons (stream-car str)
              (rec (stream-cdr str) (+ 1 i)))))
  (rec s 0))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))))))))
(define S (cons-stream 1 (merge (merge (scale-stream S 2) (scale-stream S 3))
                                (scale-stream S 5))))

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(define (integrate-series s) (stream-map / s integers))
(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s1)
                                          (stream-car s2))
                            (mul-series s1
                                        (stream-cdr s2)))))

(define (invert-unit-series s)
  (cons-stream 1
               (scale-stream (mul-series (stream-cdr s)
                                         (invert-unit-series s))
                             -1)))

(define (div-series s1 s2)
  (if (zero? (stream-car s2))
      (error "The denominator has a zero constant term: " s2)
      (mul-series s1 (invert-unit-series s2))))
(define tangent-series (div-series sine-series cosine-series))

(define (average a b) (/ (+ a b) 2.0))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define (stream-limit s tolerance)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1)))
    (if (< (abs (- s0 s1)) tolerance)
        s1
        (stream-limit (stream-cdr s) tolerance))))
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))
(define ln2-stream
  (partial-sums (ln2-summands 1)))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (interleave
     (stream-map (lambda (x) (list x (stream-car t)))
                 (stream-cdr s))
     (all-pairs (stream-cdr s) (stream-cdr t))))))

(define (louis-pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s) (stream-cdr t))))

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (stream-cdr (pairs t u)))
    (triples (stream-cdr s)
             (stream-cdr t)
             (stream-cdr u)))))
(define pythagorean-triples
  (stream-filter (lambda (t) (= (+ (square (car t))
                                   (square (cadr t)))
                                (square (caddr t))))
                 (triples integers integers integers)))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (let ((w1 (weight s1car))
                 (w2 (weight s2car)))
             (cond ((< w1 w2)
                    (cons-stream
                     s1car
                     (merge-weighted (stream-cdr s1) s2 weight)))
                   ((> w1 w2)
                    (cons-stream
                     s2car
                     (merge-weighted s1 (stream-cdr s2) weight)))
                   (else
                    (cons-stream
                     s1car
                     (merge-weighted (stream-cdr s1)
                                     (stream-cdr s2)
                                     weight)))))))))
(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define pair-a
  (weighted-pairs integers integers (lambda (p) (apply + p))))
(define (filtered? n)
  (not (or (even? n)
           (zero? (remainder n 3))
           (zero? (remainder n 5)))))
(define filtered-integers
  (stream-filter filtered? integers))
(define (pair-b-weight p)
  (let ((i (car p)) (j (cadr p)))
    (+ (* 2 i) (* 3 j) (* 5 i j))))
(define pair-b
  (weighted-pairs filtered-integers
                  filtered-integers
                  pair-b-weight))

(define (scale-pair-sum p scale)
  (+ (scale (car p)) (scale (cadr p))))
(define (cube-sum p)
  (scale-pair-sum p (lambda (x) (* x x x))))
(define (ramanujan numbers)
  (define (stream-cadr s) (stream-car (stream-cdr s)))
  (define (stream-cddr s) (stream-cdr (stream-cdr s)))
  (let ((w1 (cube-sum (stream-car numbers)))
        (w2 (cube-sum (stream-cadr numbers))))
    (if (= w1 w2)
        (cons-stream w1 (ramanujan (stream-cddr numbers)))
        (ramanujan (stream-cdr numbers)))))
(define ramanujan-numbers
  (ramanujan (weighted-pairs integers integers cube-sum)))

(define (square-sum p)
  (scale-pair-sum p (lambda (x) (* x x))))
(define (2-squares-3-ways-helper stream)
  (define (stream-cdddr s) (stream-cdr (stream-cdr (stream-cdr s))))
  (let* ((1st (stream-ref stream 0))
         (2nd (stream-ref stream 1))
         (3rd (stream-ref stream 2))
         (w1 (square-sum 1st))
         (w2 (square-sum 2nd))
         (w3 (square-sum 3rd)))
    (if (= w1 w2 w3)
        (cons-stream (list w1 1st 2nd 3rd)
                     (2-squares-3-ways (stream-cdddr stream)))
        (2-squares-3-ways (stream-cdr stream)))))
(define 2-squares-3-ways
  (2-squares-3-ways-helper (weighted-pairs integers integers square-sum)))

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand (force delayed-integrand)))
       (add-streams (scale-stream integrand dt) int))))
  int)
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (rand-update x)
  ;; ref: https://en.wikipedia.org/wiki/Linear_congruential_generator
  (let ((a 1103515245)
        (b 12345)
        (m (expt 2 31)))
    (modulo (+ (* a x) b) m)))
(define random-init (rand-update 65536))
(define random-numbers
  (cons-stream
   random-init
   (stream-map rand-update random-numbers)))
(define cesaro-stream
  (map-successive-pairs
   (lambda (r1 r2) (= (gcd r1 r2) 1))
   random-numbers))
(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))
(define pi
  (stream-map
   (lambda (p) (sqrt (/ 6 p)))
   (monte-carlo cesaro-stream 0 0)))
(show-stream pi 10)
