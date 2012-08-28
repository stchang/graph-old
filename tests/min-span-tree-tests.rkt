#lang racket

(require "../graph.rkt")
(require "../min-span-tree.rkt")
(require rackunit)

;; CLRS fig 23.1 p562
(define g
  (weighted-graph
   (a --4--  b) (a --8--  h)
   (b --8--  c) (b --11-- h)
   (c --7--  d) (c --4--  f) (c --2-- i)
   (d --9--  e) (d --14-- f)
   (e --10-- f)
   (f --2--  g)
   (g --1--  h) (g --6--  i)
   (h --7--  i)))

;; fig 23.4 p568=9
(check-equal? (mst-kruskal g)
             (weighted-graph
              (a --4-- b)
              (b --8-- c)
              (c --7-- d) (c --4-- f) (c --2-- i)
              (d --9-- e)
              (f --2-- g)
              (g --1-- h)))