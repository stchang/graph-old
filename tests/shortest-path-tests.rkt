#lang racket
(require "../shortest-path.rkt")
(require "../graph.rkt")

(require rackunit)

;;;;;;;;;; Bellman-Ford tests ;;;;;;;;;;

;; fig 24.4, p589
(define g (weighted-graph (s --6-> t) (s --7-> y)
                          (t --5-> x) (t --8-> y) (t ---4-> z)
                          (x ---2-> t)
                          (y ---3-> x) (y --9-> z)
                          (z --2-> s) (z --7-> x)))

(check-equal? 
 g
 (weighted-graph (t <-6-- s) (y <-7-- s)
                 (x <-5-- t) (y <-8-- t) (z <--4-- t)
                 (t <--2-- x)
                 (x <--3-- y) (z <-9-- y)
                 (s <-2-- z) (x <-7-- z)))

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
 (weighted-graph (t <-3-- s) (y <-5-- s)
                 (x <-6-- t) (y <-2-- t) 
                 (z <-2-- x)
                 (t <-1-- y) (x <-4-- y) (z <-6-- y)
                 (s <-3-- z) (x <-7-- z)))

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




;;;;;;;;;; dag shortest path tests ;;;;;;;;;;
;; fig 24.5, p593
(define dag (weighted-graph (r --5-> s) (r --3-> t)
                            (s --2-> t) (s --6-> x)
                            (t --7-> x) (t --4-> y) (t --2-> z)
                            (x ---1-> y) (x --1-> z)
                            (y ---2-> z)))
(check-equal?
 dag
 (weighted-graph (s <-5-- r) (t <-3-- r)
                 (t <-2-- s) (x <-6-- s)
                 (x <-7-- t) (y <-4-- t) (z <-2-- t)
                 (y <--1-- x) (z <-1-- x)
                 (z <--2-- y)))

(define-values (d-dag π-dag) (dag-shortest-path dag 's))

(check-equal? d-dag (make-hash '((r . +inf.0) (s . 0) (t . 2) (x . 6) (y . 5) (z . 3))))
(check-equal? π-dag (make-hash '((r . #f) (s . #f) (t . s) (x . s) (y . x) (z . y))))



;;;;;;;;;; dijkstra tests ;;;;;;;;;;
(define g-dij (weighted-graph (s --10-> t) (s --5-> y)
                              (t --1-> x)  (t --2-> y)
                              (x --4-> z)
                              (y --3-> t)  (y --9-> x) (y --2-> z)
                              (z --6-> x)  (z --7-> s)))

(check-equal?
 g-dij
 (weighted-graph `((s . ,(apply set '((t . 10) (y . 5))))
                   (t . ,(apply set '((x . 1)  (y . 2))))
                   (x . ,(apply set '((z . 4))))
                   (y . ,(apply set '((t . 3)  (x . 9) (z . 2))))
                   (z . ,(apply set '((x . 6)  (s . 7)))))))

(define-values (d-dij π-dij) (dijkstra g-dij 's))

(check-equal? d-dij (make-hash '((s . 0) (t . 8) (x . 9) (y . 5) (z . 7))))
(check-equal? π-dij (make-hash '((s . #f) (t . y) (x . t) (y . s) (z . y))))