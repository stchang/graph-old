#lang racket

(require "../graph.rkt")
(require rackunit)

;; tests for topological sorting (tsort)

;; using thm 22.12 p551

(define clothes
  (make-graph (undershorts -> pants) (pants -> belt) (belt -> jacket)
              (undershorts -> shoes) (pants -> shoes) (shirt -> belt)
              (shirt -> tie) (tie -> jacket) (socks -> shoes) watch))

(check-true (dag? clothes))

(define (dfs-produces-tsorted? g [dfs dfs])
  (define-values (colors d f π sorted) (dfs g))
  (for*/and ([(k vs) (in-hash g)]
             [v (in-set vs)])
    (< (hash-ref f v) (hash-ref f k))))
(check-true (dfs-produces-tsorted? clothes))
(check-true 
 (dfs-produces-tsorted? 
  clothes
  (λ (g) (dfs-with-sorting g string<? #:key symbol->string))))

(check-true (tsorted? clothes (tsort clothes)))

;; example from fig 22.8, p551
(define g (graph (m -> q) (m -> x) (m -> r)
                 (n -> q) (n -> u) (n -> o)
                 (o -> r) (o -> v) (o -> s)
                 (p -> o) (p -> s) (p -> z)
                 (q -> t)
                 (r -> u) (r -> y)
                 (s -> r)
                 t
                 (u -> t)
                 (v -> x) (v -> w)
                 (w -> z)
                 x
                 (y -> v)
                 z))

(check-true (dag? g))
(check-true (tsorted? g (tsort g)))
