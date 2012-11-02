#lang racket

(require "../graph.rkt")

(require rackunit)

;; CLRS fig 23.1 p562
(define g
  (graph
   (a --4--  b) (a --8--  h)
   (b --8--  c) (b --11-- h)
   (c --7--  d) (c --4--  f) (c --2-- i)
   (d --9--  e) (d --14-- f)
   (e --10-- f)
   (f --2--  g)
   (g --1--  h) (g --6--  i)
   (h --7--  i)))

;; fig 23.4 p568=9
#;(check-equal? (mst-kruskal g)
              (weighted-graph
               (a --4-- b)
               (b --8-- c)
               (c --7-- d) (c --4-- f) (c --2-- i)
               (d --9-- e)
               (f --2-- g)
               (g --1-- h)))

;; fig 23.5, p571
(define π (mst-prim g 'a))
(check-equal?
 (for/set ([v (in-vertices g)]) (list v (hash-ref π v)))
 ;; a is root of π
 (apply set '((a #f) (b a) (i c) (c b) (e d) (f c) (h g) (g f) (d c))))
(define π2 (mst-prim g 'd))
(check-equal?
 (for/set ([v (in-vertices g)]) (list v (hash-ref π2 v)))
 ;; d is root of π
 (apply set '((a h) (b a) (c d) (d #f) (e d) (f c) (g f) (h g) (i c))))
 ;; alternatively (from text): (apply set '((a b) (b c) (i c) (c d) (e d) (f c) (h g) (g f) (d #f))))