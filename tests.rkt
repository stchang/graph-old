#lang racket
(require "graph.rkt")

;; test bfs, example from figure 22.3, p533 of CLRS2e
(module+ test
  (require rackunit)

  ;; 
  ;; test bfs with immutable graph
  ;; 
  (define g1 (graph (r -- v) (r -- s) (s -- w) (w -- t) (w -- x) 
                    (x -- t) (t -- u) (x -- u) (x -- y) (y -- u)))
  
  ;; check equality with directed edge representation
  (check-equal?
   g1
   (graph (r -> v) (r <- v) (r -> s) (r <- s) (s -> w) (s <- w) (w -> t) (w <- t) 
          (w -> x) (w <- x) (x -> t) (x <- t) (t -> u) (t <- u) (x -> u) (x <- u) 
          (x -> y) (x <- y) (y -> u) (y <- u)))
  
  ;; check equality with literal node representation
  (check-equal? g1 (graph ((r (s v))   (v (r))       (s (r w))   (w (x t s))
                           (t (w x u)) (x (w t y u)) (u (y x t)) (y (x u)))))

  ;; check equality with assoc list representation
  (check-equal? 
   g1 
   (graph (list (cons 'r (set 's 'v))    (cons 'v (set 'r)) 
                (cons 's (set 'r 'w))    (cons 'w (set 'x 't 's))
                (cons 't (set 'w 'x 'u)) (cons 'x (set 'w 't 'y 'u))
                (cons 'u (set 'y 'x 't)) (cons 'y (set 'x 'u)))))
  ;; check equality with another assoc list representation
  (check-equal? 
   g1 
   (graph `((r . ,(set 's 'v))    (v . ,(set 'r))       (s . ,(set 'r 'w)) 
            (w . ,(set 'x 't 's)) (t . ,(set 'w 'x 'u)) (x . ,(set 'w 't 'y 'u))
            (u . ,(set 'y 'x 't)) (y . ,(set 'x 'u)))))
  
  ;; check bfs result
  (let-values ([(color dist π) (bfs g1 's)])
    (check-equal? 
     dist 
     (make-hash (list (cons 'r 1) (cons 'v 2) (cons 's 0) (cons 'w 1) 
                      (cons 't 2) (cons 'x 2) (cons 'u 3) (cons 'y 3))))
    (check-equal? 
     dist 
     (make-hash '((r . 1) (v . 2) (s . 0) (w . 1) 
                  (t . 2) (x . 2) (u . 3) (y . 3))))
    (check-equal? 
     color
     (make-hash '((r . black) (v . black) (s . black) (w . black) 
                  (t . black) (x . black) (u . black) (y . black))))
    (check-equal? 
     π
     (make-hash '((r . s) (v . r) (s . #f) (w . s) 
                  (t . w) (x . w) (u . x) (y . x)))))
  
  ;;
  ;; test bfs with mutable graph
  ;;
  (define g (make-graph))
  (add-edge! g 'r 'v)
  (add-edge! g 'r 's)
  (add-edge! g 's 'w)
  (add-edge! g 'w 't)
  (add-edge! g 'w 'x)
  (add-edge! g 'x 't)
  (add-edge! g 't 'u)
  (add-edge! g 'x 'u)
  (add-edge! g 'x 'y)
  (add-edge! g 'y 'u)
  
  ;; check equality with literal node representation
  (check-equal? 
   g 
   (make-graph ((r (s v))   (v (r))       (s (r w))   (w (x t s)) 
                (t (w x u)) (x (w t y u)) (u (y x t)) (y (x u)))))
  
  ;; check equality with assoc list representation
  (check-equal? 
   g 
   (make-graph (list (cons 'r (set 's 'v))    (cons 'v (set 'r)) 
                     (cons 's (set 'r 'w))    (cons 'w (set 'x 't 's))
                     (cons 't (set 'w 'x 'u)) (cons 'x (set 'w 't 'y 'u))
                     (cons 'u (set 'y 'x 't)) (cons 'y (set 'x 'u)))))
  
  ;; check bfs result
  (let-values ([(color dist π) (bfs g 's)])
    (check-equal? 
     dist 
     (make-hash (list (cons 'r 1) (cons 'v 2) (cons 's 0) (cons 'w 1) 
                      (cons 't 2) (cons 'x 2) (cons 'u 3) (cons 'y 3))))
    (check-equal? 
     dist 
     (make-hash '((r . 1) (v . 2) (s . 0) (w . 1) 
                          (t . 2) (x . 2) (u . 3) (y . 3))))
    (check-equal? 
     color
     (make-hash '((r . black) (v . black) (s . black) (w . black) 
                  (t . black) (x . black) (u . black) (y . black))))
    (check-equal? 
     π
     (make-hash '((r . s) (v . r) (s . #f) (w . s) 
                  (t . w) (x . w) (u . x) (y . x)))))
  
  )

;; test dfs, example from fig 22.4, p542 of CLRS2e
(module+ test
  (require rackunit)
  
  ;;
  ;; check dfs with mutable graph
  ;;
  (define g2 (make-graph))
  (add-di-edge! g2 'u 'x)
  (add-di-edge! g2 'u 'v)
  (add-di-edge! g2 'x 'v)
  (add-di-edge! g2 'v 'y)
  (add-di-edge! g2 'y 'x)
  (add-di-edge! g2 'w 'y)
  (add-di-edge! g2 'w 'z)
  (add-di-edge! g2 'z 'z)

  (check-equal? 
   g2 
   (make-graph (u -> x) (u -> v) (x -> v) (v -> y)
               (y -> x) (w -> y) (w -> z) (z -> z)))
  (check-equal? 
   g2 
   (make-graph (x <- u) (v <- u) (v <- x) (y <- v)
               (x <- y) (y <- w) (z <- w) (z <- z)))
  )