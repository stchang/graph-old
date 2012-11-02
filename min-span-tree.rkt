#lang racket

(require "graph-defs.rkt"
         "bfs.rkt"
         "queue-generic.rkt")


(provide mst-prim)

;; TODO
;; 2012-08-28
;; o) implement disjoint set data structures

;; Algorithms to calculuate Minimum Spanning Tree (MST) of a
;; connected, weighted, undirected graph
;; An MST is an acyclic subset of the edges in a graph that connects all vertices
;; (An MST always has |V|-1 edges)
;; CLRS Ch 23

;; ---------- Kruskal: --------------------------------------------------------
;; greedy algorithm to calculate MST 
;; Should ideally be O(E log V) with disjoint set data structure.
;; For now, using regular sets
#;(define (mst-kruskal G)
  (define v-sets ;; maps each vertex to a set number
    (for/hasheq ([v (get-vertices G)] [i (in-naturals)]) (values v i)))
  (define edges
    (for/fold ([res null]) ([(u vs+ws) (in-hash G)])
      (append (for/list ([v+w (in-set vs+ws)])
                (list u (car v+w) (cdr v+w)))
              res)))
  (define sorted-edges (sort edges < #:key third))
  (define-values (A new-v-sets)
    (for/fold ([A (weighted-graph)] [v-sets v-sets]) ([e sorted-edges])
      (define u (first e))
      (define v (second e))
      (define u-set (hash-ref v-sets u))
      (define v-set (hash-ref v-sets v))
      (if (= u-set v-set)
          (values A v-sets) ;; skip edge
          (values ;; add edge to A and merge sets containing u and v
           (add-weighted-edge A u v (third e))
           (for/hash ([(v set-num) (in-hash v-sets)])
             (values v (if (= set-num v-set) u-set set-num)))))))
  A
  )


;; ---------- Prim: -----------------------------------------------------------
;; greedy algorithm to calculate MST
;; Ideally, O(E log V) w binary heaps, or O(E + V log V) w Fibonacci heaps.
;; For now, using regular sets
(define (mst-prim G [root #f])
  ;; key[v] is smallest weight of any edge connecting v to (partial) mst
  (define key (make-hash)) 
  ;; π[v] is the parent of v in mst
  (define π (make-hash))
  ;; top v in Q is v not in mst with smallest weight edge connecting it to mst
  ;; ie, the next edge to add to mst
  (define Q (make-heap (λ (u v) (< (hash-ref key u) (hash-ref key v)))))
  (define Q-set (set)) ;; same elements as Q but allows set lookup and removal
  
  ;; init all weights to ∞, except root
  (if root
      (begin
        (for ([v (in-vertices G)])
          (when (not (equal? v root))
            (hash-set! key v +inf.0)
            (enqueue! Q v))
          (hash-set! π v #f)
          (set! Q-set (set-add Q-set v)))
        (hash-set! key root 0)
        (enqueue! Q root))
      (for ([v (in-vertices G)])
        (hash-set! key v +inf.0)
        (hash-set! π v #f)
        (enqueue! Q v)
        (set! Q-set (set-add Q-set v))))
  
  (define (pre-visit u) (set! Q-set (set-remove Q-set u)))
  (define (unvisited? v) (set-member? Q-set v))
  (define (visit u v wgt)
    (when (< wgt (hash-ref key v))
      (hash-set! π v u)
      (hash-set! key v wgt)
      (refresh Q)))

  (bfs-generic G Q #:unvisited? unvisited?
                   #:pre-visit pre-visit
                   #:visit visit)
  
  π
  )