#lang racket

(provide (rename-out [mk-graph graph])
         graph?
         in-neighbors in-vertices)

;; graph is a hash table mapping a vertex to a set of neighbor vertices
(struct graph (ht) #:transparent)

(define-syntax-rule (mk-graph args ...) (graph (mk-internal-hash-immut args ...)))

; graph is a immutable hash table -- in the future wrap the ht with a struct
(define-syntax (mk-internal-hash-immut stx)
  (syntax-case stx (-- -> <-)
    [(_) #'(make-immutable-hash)]
    [(_ v rest ...) 
     (identifier? #'v) 
     #'(add-vertex-internal  (mk-internal-hash-immut rest ...) 'v)]
    [(_ (u -- v) rest ...) 
     #'(add-edge-internal    (mk-internal-hash-immut rest ...) 'u 'v)]
    [(_ (u -> v) rest ...) 
     #'(add-di-edge-internal (mk-internal-hash-immut rest ...) 'u 'v)]
    [(_ (u <- v) rest ...) 
     #'(add-di-edge-internal (mk-internal-hash-immut rest ...) 'v 'u)]
    [(_ ((u (v ...)) ...)) 
     #'(make-immutable-hash (list (cons 'u (apply set '(v ...))) ...))]
    [(_ assocs) #'(make-immutable-hash assocs)]))

;; ----------------------------------------------------------------------
;; functions to add edges and vertices to graph

(define (add-vertex g v) (add-vertex-internal (graph-ht g) v))
(define (add-di-edge g u v) (add-di-edge-internal (graph-ht g) u v))
(define (add-edge g u v) (add-edge-internal (graph-ht g) u v))

;; ----------------------------------------------------------------------
;;;;;;;;;; functions to manipulate graph's internal hash table ;;;;;;;;;;

;; associate empty set with vertex v if it does not exist
(define (add-vertex-internal ht v) (hash-update ht v (λ (s) s) (set)))

  ;; add directed edge u -> v but dont try to add vertex v
(define (add-di-edge-no-vertex-check-internal ht u v) 
  (hash-update ht u (λ (s) (set-add s v)) (set)))

;; add directed edge from u to v, and make sure vertex v is in g
(define (add-di-edge-internal ht u v)
  (add-vertex-internal (add-di-edge-no-vertex-check-internal ht u v) v))

;; add undirected edge from u to v
(define (add-edge-internal ht u v)
  (add-di-edge-no-vertex-check-internal (add-di-edge-internal ht u v) v u))


;; ----------------------------------------------------------------------------
;; sequencing

(define (in-neighbors g u) (in-set (hash-ref (graph-ht g) u (set))))
(define (in-vertices g) (in-hash-keys (graph-ht g)))