#lang racket
(require "../shortest-path.rkt")
(require "../graph.rkt")

(require rackunit)


;; fig 24.4, p589
(define g (weighted-graph (s --6-> t) (s --7-> y)
                          (t --5-> x) (t --8-> y) (t ---4-> z)
                          (x ---2-> t)
                          (y ---3-> x) (y --9-> z)
                          (z --2-> s) (z --7-> x)))

(check-equal? 
 g
 (weighted-graph (s <-6-- t) (s <-7-- y)
                 (t <-5-- x) (t <-8-- y) (t <--4-- z)
                 (x <--2-- t)
                 (y <--3-- x) (y <-9-- z)
                 (z <-2-- s) (z <-7-- x)))

(define-values (d π) (bellman-ford g 's))

(check-equal? d (make-hash '((y . 7) (x . 4) (z . -2) (s . 0) (t . 2))))
(check-equal? π (make-hash '((y . s) (x . y) (z . t) (s . #f) (t . x))))

(check-equal? (get-path-to π 's) '(s))
(check-equal? (get-path-to π 's #:source 's) '(s))
(check-false (get-path-to π 's #:source 't))

(check-equal? (get-path-to π 't) '(s y x t))
(check-equal? (get-path-to π 't #:source 's) '(s y x t))
(check-equal? (get-path-to π 't #:source 't) '(t))
(check-false (get-path-to π 't #:source 'z))

(check-equal? (get-path-to π 'x) '(s y x))
(check-equal? (get-path-to π 'x #:source 's) '(s y x))
(check-equal? (get-path-to π 'x #:source 'y) '(y x))
(check-false (get-path-to π 'x #:source 'z))

(check-equal? (get-path-to π 'z) '(s y x t z))
(check-equal? (get-path-to π 'z #:source 's) '(s y x t z))
(check-equal? (get-path-to π 'z #:source 'y) '(y x t z))
(check-equal? (get-path-to π 'z #:source 'z) '(z))


;; figure 24.2, p585

(define g2 (weighted-graph (s --3-> t) (s --5-> y)
                           (t --6-> x) (t --2-> y) 
                           (x --2-> z)
                           (y --1-> t) (y --4-> x) (y --6-> z)
                           (z --3-> s) (z --7-> x)))

(check-equal? 
 g2
 (weighted-graph (s <-3-- t) (s <-5-- y)
                 (t <-6-- x) (t <-2-- y) 
                 (x <-2-- z)
                 (y <-1-- t) (y <-4-- x) (y <-6-- z)
                 (z <-3-- s) (z <-7-- x)))

(define-values (d2 π2) (bellman-ford g2 's))

(check-equal? d2 (make-hash '((y . 5) (x . 9) (z . 11) (s . 0) (t . 3))))
(check-equal? π2 (make-hash '((y . s) (x . y) (z . y) (s . #f) (t . s))))


;; figure 24.1, p583
;; negative weight cycle
(define g3 (weighted-graph (s --3-> a) (s --5-> c) (s --2-> e)
                           (a ---4-> b)
                           (b --4-> g)
                           (c --6-> d)
                           (d ---3-> c) (d --8-> g)
                           (e --3-> f)
                           (f ---6-> e) (f --7-> g)))

(check-false (bellman-ford g3 's))