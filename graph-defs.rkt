#lang racket

(require racket/generic)

(provide gen:graph)
(provide (rename-out [mk-graph graph] [mk-wgraph wgraph])
         graph?)

;; ----------------------------------------------------------------------------
;; Generic graph interface
;; ----------------------------------------------------------------------------

(define-generics graph
  (add-vertex graph node)
  (add-edge graph node1 node2 [weight])
  (add-di-edge graph from-node to-node [weight])
  (in-vertices graph)
  (in-neighbors graph node)
  (in-weighted-neighbors graph node))
;; need sorted versions of in-vertices, etc?



;; ----------------------------------------------------------------------------
;; Unweighted graphs
;; ----------------------------------------------------------------------------

;; used when weight is requested for unweighted graph
(define DEFAULT-WEIGHT 1)

;; a graph G is a 
;;   hash table mapping vertices to sets of vertices
;; [HashOf Vertex -> [SetOf Vertex]]
(struct G (ht) #:transparent
  #:methods gen:graph
  [(define (add-vertex g v) (G (add-vertex/internal (G-ht g) v)))
   (define (add-di-edge g u v [wgt DEFAULT-WEIGHT])
     (G (add-di-edge/internal (G-ht g) u v)))
   (define (add-edge g u v [wgt DEFAULT-WEIGHT])
     (G (add-edge/internal (G-ht g) u v)))
   
   (define (in-vertices g) (in-hash-keys (G-ht g)))
   (define (in-neighbors g v)
     (make-do-sequence
        (thunk
         (values
          ;; pos -> element
          (match-lambda [(list-rest v _) v])
          ;; next-pos
          rest
          ;; initial pos
          (set->list (hash-ref (G-ht g) v (set)))
          ;; termination condition
          (λ (pos) (not (null? pos)))
          #f
          #f))))
   (define (in-weighted-neighbors g v)
     (make-do-sequence
        (thunk
         (values
          ;; pos -> element
          (match-lambda [(list-rest v _) (values v DEFAULT-WEIGHT)])
          ;; next-pos
          rest
          ;; initial pos
          (set->list (hash-ref (G-ht G) v (set)))
          ;; termination condition
          (λ (pos) (not (null? pos)))
          #f
          #f))))
   ])


;; ----------------------------------------------------------------------------
;; functions to manipulate unweighted graph's internal hash table 

;; associate empty set with vertex v if it does not exist
(define (add-vertex/internal ht v) (hash-update ht v (λ (s) s) (set)))
(define (add-vertices/internal ht vs)
  (foldl (λ (v hsh) (add-vertex/internal hsh v)) ht vs))

  ;; add directed edge u -> v but dont try to add vertex v
(define (add-di-edge/internal/no-vertex-check ht u v) 
  (hash-update ht u (λ (s) (set-add s v)) (set)))

;; add directed edge from u to v, and make sure vertex v is in g
(define (add-di-edge/internal ht u v)
  (add-vertex/internal (add-di-edge/internal/no-vertex-check ht u v) v))

;; add undirected edge from u to v
(define (add-edge/internal ht u v)
  (add-di-edge/internal/no-vertex-check (add-di-edge/internal ht u v) v u))

;; unweighted graph constructor
(define-syntax-rule (mk-graph args ...) 
  (G (mk-unweighted-internal-hash args ...)))

;; macro to create internal hash table for unweighted graph
(define-syntax (mk-unweighted-internal-hash stx)
  (syntax-case stx (-- → ←)
    [(_) #'(make-immutable-hash)]
    ;; --------------------
    ;; literal syntax:
    
    ;; vertex only
    [(_ v rest ...) 
     (or (identifier? #'v) (number? (syntax->datum #'v)))
     #'(add-vertex/internal  (mk-unweighted-internal-hash rest ...) 'v)]
    
    ;; unweighted edges
    [(_ (u -- v) rest ...) 
     #'(add-edge/internal    (mk-unweighted-internal-hash rest ...) 'u 'v)]
    [(_ (u → v) rest ...) 
     #'(add-di-edge/internal (mk-unweighted-internal-hash rest ...) 'u 'v)]
    [(_ (u ← v) rest ...) 
     #'(add-di-edge/internal (mk-unweighted-internal-hash rest ...) 'v 'u)]

    ;; literal list of pairs
    [(_ (u (v ...)) ...)
     #`(add-vertices/internal 
        (make-immutable-hash (list (cons 'u (apply set '(v ...))) ...))
        '(v ... ...))]
    
    ;; --------------------
    ;; list of pairs
    [(_ assocs) #'(make-immutable-hash assocs)]))


;; ----------------------------------------------------------------------------
;; Weighted graphs
;; ----------------------------------------------------------------------------

 

;; a weighted graph WG is a 
;;   hash table mapping vertices to sets of vertex-wgt pairs
;; [HashOf Vertex -> [SetOf (cons Vertex Weight)]]
(struct WG (ht) #:transparent
  #:methods gen:graph
  [(define (add-vertex g v) (WG (add-vertex/internal (WG-ht g) v)))
   (define (add-di-edge g u v [wgt 0]) 
     (WG (add-di-wedge/internal (WG-ht g) u v wgt)))
   (define (add-edge g u v [wgt 0]) 
     (WG (add-wedge/internal (WG-ht g) u v wgt)))
   
   (define (in-vertices g) (in-hash-keys (WG-ht g)))
   (define (in-neighbors g v)
     (make-do-sequence
        (thunk
         (values
          ;; pos -> element
          (match-lambda [(list-rest (cons v _) _) v])
          ;; next-pos
          rest
          ;; initial pos
          (set->list (hash-ref (WG-ht g) v (set)))
          ;; termination condition
          (λ (pos) (not (null? pos)))
          #f
          #f))))
   (define (in-weighted-neighbors g v)
     (make-do-sequence
        (thunk
         (values
          ;; pos -> element
          (match-lambda [(list-rest (cons v wgt) _) (values v wgt)])
          ;; next-pos
          rest
          ;; initial pos
          (set->list (hash-ref (WG-ht G) v (set)))
          ;; termination condition
          (λ (pos) (not (null? pos)))
          #f
          #f))))
   ])


;; ----------------------------------------------------------------------------
;; functions to manipulate graph's internal hash table 

;; same as unweighted
;(define (add-vertex/internal ht v) (hash-update ht v (λ (s) s) (set)))

  ;; add weighted directed edge u -> v but dont try to add vertex v
(define (add-di-wedge/internal/no-vertex-check ht u v wgt) 
  (hash-update ht u (λ (s) (set-add s (cons v wgt))) (set)))

;; add weighted directed edge from u to v, and make sure vertex v is in g
(define (add-di-wedge/internal ht u v wgt)
  (add-vertex/internal (add-di-wedge/internal/no-vertex-check ht u v wgt) v))

;; add weighted undirected edge from u to v
(define (add-wedge/internal ht u v wgt)
  (add-di-wedge/internal/no-vertex-check (add-di-wedge/internal ht u v wgt) v u wgt))



(define-syntax-rule (mk-wgraph args ...) (WG (mk-internal-hash args ...)))


;; macro to create internal hash table for both weighted and unweighted graphs
(define-syntax (mk-internal-hash stx)
  (define (get-undirected-edge-weight e)
    (regexp-match 
     (regexp "(?<=^--)[0-9]+(?=--$)")
     (symbol->string (syntax->datum e))))
  (define (get-directed-edge-weight-lr e)
    (regexp-match 
     (regexp "(?<=^--)-?[0-9]+(?=->$)")
     (symbol->string (syntax->datum e))))
  (define (get-directed-edge-weight-rl e)
    (regexp-match 
     (regexp "(?<=^<-)-?[0-9]+(?=--$)")
     (symbol->string (syntax->datum e))))

  (syntax-case stx (-- -> <-)
    [(_) #'(make-immutable-hash)]
    ;; --------------------
    ;; literal syntax:
    
    ;; vertex only
    [(_ v rest ...) 
     (identifier? #'v) 
     #'(add-vertex/internal  (mk-internal-hash rest ...) 'v)]
    
    ;; unweighted edges
    [(_ (u -- v) rest ...) 
     #'(add-edge/internal    (mk-internal-hash rest ...) 'u 'v)]
    [(_ (u -> v) rest ...) 
     #'(add-di-edge/internal (mk-internal-hash rest ...) 'u 'v)]
    [(_ (u <- v) rest ...) 
     #'(add-di-edge/internal (mk-internal-hash rest ...) 'v 'u)]
    
    ;; weighted edges
    [(_ (u --wgt-- v) rest ...)
     (get-undirected-edge-weight #'--wgt--)
     (let* ([wgt (string->number (car (get-undirected-edge-weight #'--wgt--)))])
       (with-syntax ([wgt (datum->syntax #'--wgt-- wgt)])
         #'(add-wedge/internal (mk-internal-hash rest ...) 'u 'v wgt)))]
    [(_ (u --wgt-> v) rest ...)
     (get-directed-edge-weight-lr #'--wgt->)
     (let* ([wgt (string->number (car (get-directed-edge-weight-lr #'--wgt->)))])
       (with-syntax ([wgt (datum->syntax #'--wgt-> wgt)])
         #'(add-di-wedge/internal (mk-internal-hash rest ...) 'u 'v wgt)))]
    [(_ (u <-wgt-- v) rest ...)
     (get-directed-edge-weight-rl #'<-wgt--)
     (let* ([wgt (string->number (car (get-directed-edge-weight-rl #'<-wgt--)))])
       (with-syntax ([wgt (datum->syntax #'<-wgt-- wgt)])
         #'(add-di-wedge/internal (mk-internal-hash rest ...) 'v 'u wgt)))]

    ;; literal list of pairs
    [(_ ((u ((v wgt) ...)) ...)) 
     #'(make-immutable-hash (list (cons 'u (apply set '((v . wgt) ...))) ...))]
    [(_ ((u (v ...)) ...)) 
     #`(make-immutable-hash (list (cons 'u (apply set '(v ...))) ...))]
    
    ;; --------------------
    ;; list of pairs
    [(_ assocs) #'(make-immutable-hash assocs)]))

