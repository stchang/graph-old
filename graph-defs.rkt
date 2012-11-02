#lang racket

(require racket/generic)

(provide (rename-out [mk-graph graph])
         in-neighbors in-vertices)
       ;  in-neighbors in-vertices)

;; ----------------------------------------------------------------------------
;; Generic graph interface

(define-generics graph
  (add-vertex graph node)
  (add-edge graph node1 node2 [weight])
  (add-di-edge graph from-node to-node [weight])
  (in-vertices graph))

(define DEF-WGT 1)
(define-for-syntax DEF-WGT 1)

;; a graph G is a hash table mapping a vertex to a set of vertex-weight pairs
;; [HashOf Vertex [SetOf (cons Vertex Weight)]]
(struct G (ht) #:transparent
  #:methods gen:graph
  [(define (add-vertex g v) (add-vertex/internal (G-ht g) v))
   (define (add-di-edge g u v [wgt DEF-WGT]) (add-di-edge/internal (G-ht g) u v wgt))
   (define (add-edge g u v [wgt DEF-WGT]) (add-edge/internal (G-ht g) u v wgt))
   
   (define (in-vertices g) (in-hash-keys (G-ht g)))])

(define-syntax (in-weighted-neighbors stx)
  (syntax-case stx ()
    [(_ G u)
     ;; Pos is [ListOf (cons v wgt)]
     #'(make-do-sequence
        (thunk
         (values
          ;; pos -> element
          (match-lambda [(list (cons v wgt) rst (... ...)) (values v wgt)])
          ;; next-pos
          rest
          ;; initial pos
          (set->list (hash-ref (G-ht G) u (set)))
          ;; termination condition
          (λ (pos) (not (null? pos)))
          #f
          #f)))]))
(define-syntax (in-unweighted-neighbors stx)
  (syntax-case stx ()
    [(_ G u)
     ;; Pos is [ListOf (cons v wgt)]
     #'(make-do-sequence
        (thunk
         (values
          ;; pos -> element
          (match-lambda [(list (cons v wgt) rst (... ...)) v])
          ;; next-pos
          rest
          ;; initial pos
          (set->list (hash-ref (G-ht G) u (set)))
          ;; termination condition
          (λ (pos) (not (null? pos)))
          #f
          #f)))]))

(define-sequence-syntax in-neighbors
  (λ () #'(λ (g u) (for/list ([(v wgt) (in-neighbors g u)]) (list v wgt))))
  (λ (stx)
    (syntax-case stx ()
      [[(v wgt) (_ g u)]
       #'[(v wgt) (in-weighted-neighbors g u)]]
      [[v (_ g u)]
       #'[v (in-unweighted-neighbors g u)]])))

(define-syntax-rule (mk-graph args ...) (G (mk-internal-hash args ...)))

; graph is a immutable hash table -- in the future wrap the ht with a struct
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
         #'(add-edge/internal (mk-internal-hash rest ...) 'u 'v wgt)))]
    [(_ (u --wgt-> v) rest ...)
     (get-directed-edge-weight-lr #'--wgt->)
     (let* ([wgt (string->number (car (get-directed-edge-weight-lr #'--wgt->)))])
       (with-syntax ([wgt (datum->syntax #'--wgt-> wgt)])
         #'(add-di-edge/internal (mk-internal-hash rest ...) 'u 'v wgt)))]
    [(_ (u <-wgt-- v) rest ...)
     (get-directed-edge-weight-rl #'<-wgt--)
     (let* ([wgt (string->number (car (get-directed-edge-weight-rl #'<-wgt--)))])
       (with-syntax ([wgt (datum->syntax #'<-wgt-- wgt)])
         #'(add-di-edge/internal (mk-internal-hash rest ...) 'v 'u wgt)))]

    ;; literal list of pairs
    [(_ ((u ((v wgt) ...)) ...)) 
     #'(make-immutable-hash (list (cons 'u (apply set '((v . wgt) ...))) ...))]
    [(_ ((u (v ...)) ...)) 
     #`(make-immutable-hash (list (cons 'u (apply set '((v . #,DEF-WGT) ...))) ...))]
    
    ;; --------------------
    ;; list of pairs
    [(_ assocs) #'(make-immutable-hash assocs)]))

;; ----------------------------------------------------------------------------
;; functions to manipulate graph's internal hash table 

;; associate empty set with vertex v if it does not exist
(define (add-vertex/internal ht v) (hash-update ht v (λ (s) s) (set)))

  ;; add directed edge u -> v but dont try to add vertex v
(define (add-di-edge/internal/no-vertex-check ht u v [wgt DEF-WGT]) 
  (hash-update ht u (λ (s) (set-add s (cons v wgt))) (set)))

;; add directed edge from u to v, and make sure vertex v is in g
(define (add-di-edge/internal ht u v [wgt DEF-WGT])
  (add-vertex/internal (add-di-edge/internal/no-vertex-check ht u v wgt) v))

;; add undirected edge from u to v
(define (add-edge/internal ht u v [wgt DEF-WGT])
  (add-di-edge/internal/no-vertex-check (add-di-edge/internal ht u v wgt) v u wgt))

