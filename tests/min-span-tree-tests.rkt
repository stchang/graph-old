#lang racket

(require "../graph.rkt")
(require "../min-span-tree.rkt")
(require rackunit)

(weighted-graph
 (a --4--  b) (a --8--  h)
 (b --8--  c) (b --11-- h)
 (c --7--  d) (c --4--  f) (c --2-- i)
 (d --9--  e) (d --14-- f)
 (e --10-- f)
 (f --2--  g)
 (g --1--  h) (g --6--  i)
 (h --7--  i))