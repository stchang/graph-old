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

;; test dfs
(module+ test
  (require rackunit)
  
  ;;
  ;; check dfs with mutable graph, example from fig 22.4, p542 of CLRS2e
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
  
  (check-equal?
   g2
   (make-graph ((x (v)) (u (v x)) (v (y)) (y (x)) (w (y z)) (z (z)))))
  

  ;; this is here to see the ordering of nodes
  ;(check-equal? (hash-keys g2) '(z u w x v y))
  (define (parens-thm? g)
    ; do dfs
    (define-values (color d f π sorted) (dfs g))
    (define keys (hash-keys g))

    ; predicates for parens thm
    (define (valid? u v)
      (and (< (hash-ref d u) (hash-ref f u))
           (< (hash-ref d v) (hash-ref f v))))
    ;; true if u is a descendant of v
    (define (descendant? u v)
      (and u
           (or (equal? (hash-ref π u) v)
               (descendant? (hash-ref π u) v))))
    (define (disjoint? u v)
      (and (or (< (hash-ref f u) (hash-ref d v))
               (< (hash-ref f v) (hash-ref d u)))
           (not (descendant? u v))
           (not (descendant? v u))))
    (define (within? u v)
      (and (< (hash-ref d v) (hash-ref d u))
           (< (hash-ref f u) (hash-ref f v))
           (descendant? u v)))
    (define (nesting? u v)
      (define d? (descendant? v u))
      (define <? (< (hash-ref d u) (hash-ref d v) (hash-ref f v) (hash-ref f u)))
      (or (and d? <?)
          (and (not d?) (not <?))))

    (and
     (equal? color (make-hash (map (λ (k) (cons k 'black)) keys)))
                               
    ; testing these dont work because it depends on order returned by hash-keys
    ;(equal? d (make-hash '((u . 3)  (v . 5) (x . 4) (y . 6) (w . 11) (z . 1))))
    ;(equal? f (make-hash '((u . 10) (v . 8) (x . 9) (y . 7) (w . 12) (z . 2))))
    ;(equal? π (make-hash '((u . #f) (v . x) (x . u) (y . v) (w . #f) (z . w))))
    
     ; but we can test "parenthesis theorem" (p543 thm 22.7)
     (for*/and ([u keys] [v keys])
       ;(printf "[d[u],f[u]]=[~a,~a]\n" (hash-ref d u) (hash-ref f u))
       ;(printf "[d[v],f[v]]=[~a,~a]\n" (hash-ref d v) (hash-ref f v))
       (or (equal? u v)
           (and (valid? u v)
                (or (disjoint? u v)
                    (within? u v)
                    (within? v u))
                (nesting? u v))))))
  
    
  (check-true (parens-thm? g))

  
  
  
  ;;
  ;; check dfs with mutable graph, example from fig 22.5, p544
  ;;
  (define g3 (make-graph))
  (add-di-edge! g3 'y 'x)
  (add-di-edge! g3 'x 'z)
  (add-di-edge! g3 'z 'y)
  (add-di-edge! g3 'z 'w)
  (add-di-edge! g3 'w 'x)
  (add-di-edge! g3 's 'z)
  (add-di-edge! g3 's 'w)
  (add-di-edge! g3 'v 's)
  (add-di-edge! g3 'v 'w)
  (add-di-edge! g3 't 'v)
  (add-di-edge! g3 't 'u)
  (add-di-edge! g3 'u 'v)
  (add-di-edge! g3 'u 't)

  (check-equal? 
   g3 
   (make-graph (y -> x) (x -> z) (z -> w) (s -> z) (v -> s) (t -> u) (u -> t)
               (w -> x)          (z -> y) (s -> w) (v -> w) (t -> v) (u -> v)))
  (check-equal? 
   g3 
   (make-graph (x <- y) (z <- x) (w <- z) (z <- s) (s <- v) (u <- t) (t <- u)
               (x <- w)          (y <- z) (w <- s) (w <- v) (v <- t) (v <- u)))
  
  
  (check-equal?
   g3
   (make-graph ((y (x)) (x (z)) (z (y w)) (w (x)) (s (z w)) (v (s w)) (t (v u)) (u (t v)))))
  
  (check-true (parens-thm? g3))

  )


;; test tsort
;; using thm 22.12 p551
(module+ test
  (define clothes
    (make-graph (undershorts -> pants) (pants -> belt) (belt -> jacket)
                (undershorts -> shoes) (pants -> shoes) (shirt -> belt)
                (shirt -> tie) (tie -> jacket) (socks -> shoes) watch))
  (define (dfs-produces-tsort? g)
    (define-values (colors d f π sorted) (dfs g))
    (for*/and ([k (in-hash-keys g)]
               [v (in-set (hash-ref g k))])
      (< (hash-ref f v) (hash-ref f k))))
  (dfs-produces-tsort? clothes)
  )