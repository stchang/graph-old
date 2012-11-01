#lang racket

(require racket/generic)

(provide (rename-out [mk-graph graph] [mk-weighted-graph weighted-graph])
         in-neighbors in-vertices)
       ;  in-neighbors in-vertices)

;; ----------------------------------------------------------------------------
;; Generic graph interface

(define-generics graph
  (add-vertex graph node)
  (add-edge graph node1 node2 [weight])
  (add-di-edge graph from-node to-node [weight])
  (in-vertices graph)
  (in-neighbors graph node)
  (in-neighbors/weighted graph node))

;; ----------------------------------------------------------------------------
;; unweighted graphs

;; graph is a hash table mapping a vertex to a set of neighbor vertices
(struct G (ht) #:transparent
  #:methods gen:graph
  [(define (add-vertex g v) (add-vertex-internal (G-ht g) v))
   (define (add-di-edge g u v [wgt 1]) (add-di-edge-internal (G-ht g) u v))
   (define (add-edge g u v [wgt 1]) (add-edge-internal (G-ht g) u v))
   
   (define (in-vertices g) (in-hash-keys (G-ht g)))
   (define (in-neighbors g u) (in-set (hash-ref (G-ht g) u (set))))])

(define-syntax-rule (mk-graph args ...) (G (mk-internal-hash-immut args ...)))

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

;; ----------------------------------------------------------------------------
;; functions to manipulate graph's internal hash table 

;; associate empty set with vertex v if it does not exist
(define (add-vertex-internal ht v) (hash-update ht v (λ (s) s) (set)))

  ;; add directed edge u -> v but dont try to add vertex v
(define (add-di-edge-internal/no-vertex-check ht u v) 
  (hash-update ht u (λ (s) (set-add s v)) (set)))

;; add directed edge from u to v, and make sure vertex v is in g
(define (add-di-edge-internal ht u v)
  (add-vertex-internal (add-di-edge-internal/no-vertex-check ht u v) v))

;; add undirected edge from u to v
(define (add-edge-internal ht u v)
  (add-di-edge-internal/no-vertex-check (add-di-edge-internal ht u v) v u))


;; ----------------------------------------------------------------------------
;; weighted graph

(struct G/wgt (ht) #:transparent
  #:methods gen:graph
  [(define (add-vertex g v) (add-vertex-internal (G/wgt-ht g) v))
   (define (add-di-edge g u v [wgt 0]) (add-weighted-di-edge-internal (G/wgt-ht g) u v wgt))
   (define (add-edge g u v [wgt 0]) (add-weighted-edge-internal (G/wgt-ht g) u v wgt))
   
   (define (in-vertices g) (in-hash-keys (G/wgt-ht g)))
   (define (in-neighbors g u) (in-set (hash-ref (G/wgt-ht g) u (set))))])

(define-syntax-rule (mk-weighted-graph args ...) (G/wgt (mk-weighted-internal-hash-immut args ...)))

; weighted-graph (copied from graph): uses immutable hash table
(define-syntax (mk-weighted-internal-hash-immut stx)
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
  (syntax-case stx (-> <-)
    [(_) #'(make-immutable-hash)]
    [(_ v rest ...) 
     (identifier? #'v) 
     #'(add-vertex-internal (mk-weighted-internal-hash-immut rest ...) 'v)]
    [(_ (u --wgt-- v) rest ...)
     (get-undirected-edge-weight #'--wgt--)
     (let* ([wgt (string->number (car (get-undirected-edge-weight #'--wgt--)))])
       (with-syntax ([wgt (datum->syntax #'--wgt-- wgt)])
       #'(add-weighted-edge-internal (mk-weighted-internal-hash-immut rest ...) 'u 'v wgt)))]
    [(_ (u --wgt-> v) rest ...)
     (get-directed-edge-weight-lr #'--wgt->)
     (let* ([wgt (string->number (car (get-directed-edge-weight-lr #'--wgt->)))])
       (with-syntax ([wgt (datum->syntax #'--wgt-> wgt)])
         #'(add-weighted-di-edge-internal (mk-weighted-internal-hash-immut rest ...) 'u 'v wgt)))]
    [(_ (u <-wgt-- v) rest ...)
     (get-directed-edge-weight-rl #'<-wgt--)
     (let* ([wgt (string->number (car (get-directed-edge-weight-rl #'<-wgt--)))])
       (with-syntax ([wgt (datum->syntax #'<-wgt-- wgt)])
         #'(add-weighted-di-edge-internal (mk-weighted-internal-hash-immut rest ...) 'v 'u wgt)))]
    [(_ ((u (v ...)) ...)) 
     #'(make-immutable-hash (list (cons 'u (apply set '(v ...))) ...))]
    [(_ assocs) #'(make-immutable-hash assocs)]))

(define (add-weighted-di-edge-internal ht u v w)
  (add-vertex-internal (add-weighted-di-edge-internal/no-vertex-check ht u v w) v))
(define (add-weighted-di-edge-internal/no-vertex-check ht u v w)
  (hash-update ht u (λ (s) (set-add s (cons v w))) (set)))
(define (add-weighted-edge-internal ht u v w)
  (add-weighted-di-edge-internal/no-vertex-check (add-weighted-di-edge-internal ht u v w) v u w))