#lang racket

(require rackunit)

(require "../graph.rkt")

;; test bfs, example from figure 22.3, p533 of CLRS2e

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
 (graph (list (cons 'r (set '(s . 1) '(v . 1)))    
              (cons 's (set '(r . 1) '(w . 1))) 
              (cons 't (set '(w . 1) '(x . 1) '(u . 1)))   
              (cons 'u (set '(y . 1) '(x . 1) '(t . 1)))
              (cons 'v (set '(r . 1))) 
              (cons 'w (set '(x . 1) '(t . 1) '(s . 1)))
              (cons 'x (set '(w . 1) '(t . 1) '(y . 1) '(u . 1))) 
              (cons 'y (set '(x . 1) '(u . 1))))))
;; check equality with another assoc list representation
(check-equal? 
 g1 
 (graph `((r . ,(set '(s . 1) '(v . 1)))   
          (s . ,(set '(r . 1) '(w . 1)))
          (t . ,(set '(w . 1) '(x . 1) '(u . 1))) 
          (u . ,(set '(y . 1) '(x . 1) '(t . 1)))
          (v . ,(set '(r . 1)))
          (w . ,(set '(x . 1) '(t . 1) '(s . 1)))
          (x . ,(set '(w . 1) '(t . 1) '(y . 1) '(u . 1))) 
          (y . ,(set '(x . 1) '(u . 1))))))
  
;;; g1 not dag because it's undirected
;(check-false (dag? g1))

;; check bfs result
(let-values ([(dist π) (bfs g1 's)])
  (check-equal? 
   dist 
   (make-hash (list (cons 'r 1) (cons 'v 2) (cons 's 0) (cons 'w 1) 
                    (cons 't 2) (cons 'x 2) (cons 'u 3) (cons 'y 3))))
  (check-equal? 
   dist 
   (make-hash '((r . 1) (v . 2) (s . 0) (w . 1) 
                (t . 2) (x . 2) (u . 3) (y . 3))))
  #;(check-equal? 
   color
   (make-hash '((r . black) (v . black) (s . black) (w . black) 
                (t . black) (x . black) (u . black) (y . black))))
  (check-equal? 
   π
   (make-hash '((r . s) (v . r) (s . #f) (w . s) 
                (t . w) (x . w) (u . x) (y . x)))))


;;; test dfs
;(module+ test
;  (require rackunit)
;  
;  ;;
;  ;; check dfs with mutable graph, example from fig 22.4, p542 of CLRS2e
;  ;;
;  (define g2 (make-graph))
;  (add-di-edge! g2 'u 'x)
;  (add-di-edge! g2 'u 'v)
;  (add-di-edge! g2 'x 'v)
;  (add-di-edge! g2 'v 'y)
;  (add-di-edge! g2 'y 'x)
;  (add-di-edge! g2 'w 'y)
;  (add-di-edge! g2 'w 'z)
;  (add-di-edge! g2 'z 'z)
;
;  (check-equal? 
;   g2 
;   (make-graph (u -> x) (u -> v) (x -> v) (v -> y)
;               (y -> x) (w -> y) (w -> z) (z -> z)))
;  (check-equal? 
;   g2 
;   (make-graph (x <- u) (v <- u) (v <- x) (y <- v)
;               (x <- y) (y <- w) (z <- w) (z <- z)))
;  
;  (check-equal?
;   g2
;   (make-graph ((x (v)) (u (v x)) (v (y)) (y (x)) (w (y z)) (z (z)))))
;  
;  (check-false (dag? g2))
;  
;  ;; this is here to see the ordering of nodes
;  ;(check-equal? (hash-keys g2) '(z u w x v y))
;  (define (parens-thm? g [dfs dfs])
;    ; do dfs
;    (define-values (color d f π) (dfs g))
;    (define keys (hash-keys g))
;
;    ; predicates for parens thm
;    (define (valid? u v)
;      (and (< (hash-ref d u) (hash-ref f u))
;           (< (hash-ref d v) (hash-ref f v))))
;    ;; true if u is a descendant of v
;    ;; NB: this function only determines "descendants" according to π from dfs
;    ;; It's not the same as saying there is a path from v to u
;    ;; It may be the case that there is a path but descendant? is false.
;    (define (descendant? u v)
;      (and u
;           (or (equal? (hash-ref π u) v)
;               (descendant? (hash-ref π u) v))))
;    (define (disjoint? u v)
;      (and (or (< (hash-ref f u) (hash-ref d v))
;               (< (hash-ref f v) (hash-ref d u)))
;           (not (descendant? u v))
;           (not (descendant? v u))))
;    (define (within? u v)
;      (and (< (hash-ref d v) (hash-ref d u))
;           (< (hash-ref f u) (hash-ref f v))
;           (descendant? u v)))
;    (define (nesting? u v)
;      (define d? (descendant? v u))
;      (define <? (< (hash-ref d u) (hash-ref d v) (hash-ref f v) (hash-ref f u)))
;      (or (and d? <?)
;          (and (not d?) (not <?))))
;
;    (and
;     (equal? color (make-hash (map (λ (k) (cons k 'black)) keys)))
;                               
;     ; "parenthesis theorem" (p543 thm 22.7)
;     (for*/and ([u keys] [v keys])
;       (or (equal? u v)
;           (and (valid? u v)
;                (or (disjoint? u v)
;                    (within? u v)
;                    (within? v u))
;                (nesting? u v))))))  
;      
;  (check-true (parens-thm? g2))
;  (check-true (parens-thm? g2 (λ (grph) 
;                               (dfs-with-sorting 
;                                grph string<? #:key symbol->string))))
;  
;  ;; check exact result of dfs-with-sorting on g2 
;  ;; (alphabetical order, like in book p542)
;  (let-values ([(colors d f π) 
;                (dfs-with-sorting g2 string<? #:key symbol->string)])
;    (check-true
;     (for/and ([v (in-hash-values colors)])
;       (equal? v 'black)))
;    (check-equal? 
;     d
;     (make-graph 
;      '((v . 2) (w . 9) (y . 3) (u . 1) (x . 4) (z . 10))))
;    (check-equal?
;     f
;     (make-graph
;      '((u . 8) (w . 12) (x . 5) (v . 7) (y . 6) (z . 11))))
;    (check-equal?
;     π
;     (make-graph
;      '((v . u) (y . v) (x . y) (u . #f) (w . #f) (z . w))))
;    ;; sorted doesnt matter because g2 is not dag
;;    (check-equal? sorted '(w z u v y x))
;    )
;  
; 
;  
;  
;  ;;
;  ;; check dfs with mutable graph, example from fig 22.5, p544
;  ;;
;  (define g3 (make-graph))
;  (add-di-edge! g3 'y 'x)
;  (add-di-edge! g3 'x 'z)
;  (add-di-edge! g3 'z 'y)
;  (add-di-edge! g3 'z 'w)
;  (add-di-edge! g3 'w 'x)
;  (add-di-edge! g3 's 'z)
;  (add-di-edge! g3 's 'w)
;  (add-di-edge! g3 'v 's)
;  (add-di-edge! g3 'v 'w)
;  (add-di-edge! g3 't 'v)
;  (add-di-edge! g3 't 'u)
;  (add-di-edge! g3 'u 'v)
;  (add-di-edge! g3 'u 't)
;
;  (check-equal? 
;   g3 
;   (make-graph (y -> x) (x -> z) (z -> w) (s -> z) (v -> s) (t -> u) (u -> t)
;               (w -> x)          (z -> y) (s -> w) (v -> w) (t -> v) (u -> v)))
;  (check-equal? 
;   g3 
;   (make-graph (x <- y) (z <- x) (w <- z) (z <- s) (s <- v) (u <- t) (t <- u)
;               (x <- w)          (y <- z) (w <- s) (w <- v) (v <- t) (v <- u)))
;  
;  
;  (check-equal?
;   g3
;   (make-graph ((y (x)) (x (z)) (z (y w)) (w (x)) (s (z w)) (v (s w)) (t (v u)) (u (t v)))))
;  
;  (check-false (dag? g3))
;  
;  (check-true (parens-thm? g3))
;  (check-true (parens-thm? g3 (λ (grph) (dfs-with-sorting grph string<? #:key symbol->string))))
;  
;  ;; check exact result of dfs-with-sorting on g3
;  ;; (alphabetical order)
;  (let-values ([(colors d f π) 
;                (dfs-with-sorting g3 string<? #:key symbol->string)])
;    (check-true
;     (for/and ([v (in-hash-values colors)])
;       (equal? v 'black)))
;    (check-equal? 
;     d
;     (make-graph 
;      '((u . 12) (w . 2) (z . 4) (v . 13) (s . 1) (y . 5) (t . 11) (x . 3))))
;    (check-equal?
;     f
;     (make-graph
;      '((v . 14) (w . 9) (z . 7) (u . 15) (s . 10) (x . 8) (t . 16) (y . 6))))
;    (check-equal?
;     π
;     (make-graph
;      '((v . u) (w . s) (z . x) (u . t) (s . #f) (x . w) (t . #f) (y . z))))
;    ;; sorted doesnt matter because g3 is not dag
;;    (check-equal? sorted '(t u v s w x z y))
;    )
;  )
;
;
;
;
;;; todo:
;;; add dfs example from fig 22.6
;;; add tsort example from fig 22.8
;;; impl graph as struct, but add printing property so it displays the hash
;;; start documentation