#lang racket

;; tests for strongly connected components

(require "../graph.rkt")
(require rackunit)

;; from fig 22.9 p 553
(define g (graph (a -> b)
                 (b -> c) (b -> e) (b -> f)
                 (c -> d) (c -> g)
                 (d -> c) (d -> h)
                 (e -> a) (e -> f)
                 (f -> g)
                 (g -> f) (g -> h)
                 (h -> h)))

(check-equal?
 (transpose (add-vertex (add-vertex g 'i) 'j))
 (graph (a <- b)
        (b <- c) (b <- e) (b <- f)
        (c <- d) (c <- g)
        (d <- c) (d <- h)
        (e <- a) (e <- f)
        (f <- g)
        (g <- f) (g <- h)
        (h <- h)
        i j))