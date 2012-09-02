#lang racket
(require "graph.rkt")

(provide bellman-ford)

;; single source shortest path algorithms
;; graph g cannot have negative weight cycle reachable from source s

(define-syntax (relax stx)
  (syntax-case stx ()
    [(_ u v wgt)
     (with-syntax ([d (datum->syntax stx 'd)]
                   [π (datum->syntax stx 'π)])
       #'(let ([du+wgt (+ (hash-ref d u) wgt)])
           (when (> (hash-ref d v) du+wgt)
             (hash-set! d v du+wgt)
             (hash-set! π v u))))]))

;; Bellman-Ford
;; negative edge weights allowed, but no negative weight cycles reachable from source s
;; O(VE)
;; returns #f is G has negative weight cycle
;; ow returns hashes d π where d[v] is weight of shortest path from s to v
;; and π[v] is predecessor tree for vertex v
(define (bellman-ford G s)
  (define d (make-hash))
  (define π (make-hash))
  
  (define vs (get-vertices G))
  
  ;; init single source 
  (for ([v vs])
    (hash-set! d v +inf.0)
    (hash-set! π v #f))
  (hash-set! d s 0)
  
  ;; find shortest paths
  (for ([_  (cdr vs)]) ;; 1 to |V[G]|-1
    (for ([(u v wgt) (in-edges G)])
      (relax u v wgt)))
  
  ;; detect negative weight cycle
  (if (for/or ([(u v wgt) (in-edges G)])
        (> (hash-ref d v) (+ (hash-ref d u) wgt)))
      #f
      (values d π)))
  
  
  
  
  
;; Dijkstra
;; graph g must have all nonnegative edge weights

