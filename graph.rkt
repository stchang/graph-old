#lang racket

(require data/queue)

(provide graph make-graph 
         add-edge add-di-edge add-edge! add-di-edge! add-vertex add-vertex!
         bfs dfs dfs-with-sorting tsort dag?)

;; TODO:
;; 2012-07-15
;; o) wrap hash table in graph struct
;; o) add grapheq and grapheqv versions corresponding to hasheq and hasheqv
;; 2012-07-28
;; x) add dag? pred - DONE 2012-08-12
;; 2012-08-12
;; o) generalize dfs and bfs
;;    - for example, dag? has code copied from dfs

; 2012-07-15: not used -- graph is currently a hash table
#;(define-struct graph (ht)
  #:omit-define-syntaxes)

;; macro to make macro definition of graph
;; allows using immutable or mutable hashes
#;(define-syntax (make-graph-definition stx)
  (syntax-case stx ()
    [(_ mk-graph hash-fn add-edge add-di-edge)
     #'(define-syntax (mk-graph stx)
         (syntax-case stx (-- ->)
           [(_) #'(hash-fn)]
           [(_ (u -- v) rest (... ...)) #'(add-edge    (mk-graph rest (... ...)) 'u 'v)]
           [(_ (u -> v) rest (... ...)) #'(add-di-edge (mk-graph rest (... ...)) 'u 'v)]
           [(_ (u <- v) rest (... ...)) #'(add-di-edge (mk-graph rest (... ...)) 'v 'u)]
           [(_ ((u (v (... ...))) (... ...))) 
            #'(hash-fn (list (cons 'u (apply set '(v (... ...)))) (... ...)))]
           [(_ assocs) #'(hash-fn assocs)]))]))

; graph is a immutable hash table -- in the future wrap the ht with a struct
(define-syntax (graph stx)
  (syntax-case stx (-- ->)
    [(_) #'(make-immutable-hash)]
    [(_ v rest ...) (identifier? #'v) #'(add-vertex (graph rest ...) 'v)]
    [(_ (u -- v) rest ...) #'(add-edge    (graph rest ...) 'u 'v)]
    [(_ (u -> v) rest ...) #'(add-di-edge (graph rest ...) 'u 'v)]
    [(_ (u <- v) rest ...) #'(add-di-edge (graph rest ...) 'v 'u)]
    [(_ ((u (v ...)) ...)) 
     #'(make-immutable-hash (list (cons 'u (apply set '(v ...))) ...))]
    [(_ assocs) #'(make-immutable-hash assocs)]))

; make graph with mutable hash table
(define-syntax (make-graph stx)
  (syntax-case stx (-- ->)
    [(_) #'(make-hash)]
    [(_ v rest ...) (identifier? #'v) #'(graph->mutable (graph v rest ...))]
    [(_ (u -- v) rest ...) #'(graph->mutable (graph (u -- v) rest ...))]
    [(_ (u -> v) rest ...) #'(graph->mutable (graph (u -> v) rest ...))]
    [(_ (u <- v) rest ...) #'(graph->mutable (graph (u <- v) rest ...))]
    [(_ ((u (v ...)) ...)) 
     #'(make-hash (list (cons 'u (apply set '(v ...))) ...))]
    [(_ assocs) #'(make-hash assocs)]))

(define (graph->mutable g)
  (define mut-g (make-graph))
  (for ([(k vs) (in-hash g)])
    (if (set-empty? vs)
        (add-vertex! mut-g k)
        (for ([v (in-set vs)])
          (add-di-edge! mut-g k v))))
  mut-g)
    

;; associate empty set with vertex v is it does not exist
(define (add-vertex g v) (hash-update g v (λ (s) s) (set)))
(define (add-vertex! g v) (hash-update! g v (λ (s) s) (set)))
  
;; add edge u -> v but dont try to add vertex v
(define (add-di-edge-no-vertex-check g u v) 
  (hash-update g u (λ (s) (set-add s v)) (set)))

;; add-di-edge : Graph Node Node -> Graph
;; add directed edge from u to v, functionally
;; and make sure vertex v is in g
(define (add-di-edge g u v)
  (define ht g #;(graph-ht g))
  #;(hash-set ht u (set-add (hash-ref ht u (set)) v))
  (add-vertex (add-di-edge-no-vertex-check g u v) v))

;; add-edge : Graph Node Node -> Graph
;; add edge from u to v, functionally
(define (add-edge g u v)
  (add-di-edge-no-vertex-check (add-di-edge g u v) v u))

;; add edge u->v (imperatively) but dont try to add vertex v
(define (add-di-edge!-no-vertex-check g u v)
  (hash-update! g u (λ (s) (set-add s v)) (set)))

;; add-di-edge! : Graph Node Node -> void
;; add directed edge from u to v, imperatively
(define (add-di-edge! g u v)
  (add-di-edge!-no-vertex-check g u v)
  (add-vertex! g v))

;; add-edge! : Graph Node Node -> void
;; add edge from u to v, imperatively
(define (add-edge! g u v)
  (add-di-edge! g u v)
  (add-di-edge!-no-vertex-check g v u))


;; Color is 'white or 'gray or 'black

;; Performs breadth-first search on graph g and source s
;; bfs : Graph Node -> (values [HashOf (Node -> Color)]
;;                             [HashOf (Node -> Number)]
;;                             [HashOf (Node -> Node)])
;; algorithm from p532 CLRS2e
(define (bfs g s)
  (define color (make-hash))
  (define dist (make-hash))
  (define π (make-hash))
  (define Q (make-queue))
  
  ;; init
  (for ([u (in-hash-keys g)])
    (if (equal? s u)
        (begin
          (hash-set! color u 'gray)
          (hash-set! dist u 0))
        (begin
          (hash-set! color u 'white)
          (hash-set! dist u +inf.0)))
    (hash-set! π u #f))
  (enqueue! Q s)
  
  ;; do search
  (do () ((queue-empty? Q) (values color dist π))
    (let ([u (dequeue! Q)])
      (for ([v (in-set (hash-ref g u))])
        (when (eq? 'white (hash-ref color v))
          (hash-set! color v 'gray)
          (hash-set! dist v (add1 (hash-ref dist u)))
          (hash-set! π v u)
          (enqueue! Q v)))
      (hash-set! color u 'black))))


;; Performs depth-first search on graph g
;; algorithm from p541 CLRS2e
;; Alternative (functional) dfs algorithm uses a "seen" list/set
;;  but this adds O(n), n = # vertices, at every step to check the seen list
(define (dfs g)
  (define color (make-hash))
  ;; d and f map vertices to timestamps
  (define d (make-hash)) ;; d[v] = when vertex v discovered (v gray)
  (define f (make-hash)) ;; f[v] = when search finishes v's adj list (v black)
  (define π (make-hash))
  (define time 0)
  (define tsorted null)
  (define vertices (hash-keys g))
  ;; init
  (for ([u vertices])
    (hash-set! color u 'white)
    (hash-set! π u #f))
  
  ;; do search
  (for ([u vertices])
    (when (eq? (hash-ref color u) 'white)
      ;; visit vertex u
      (let VISIT ([u u])
        (hash-set! color u 'gray)
        (set! time (add1 time))
        (hash-set! d u time)
        (for ([v (in-set (hash-ref g u))])
          (when (eq? (hash-ref color v) 'white)
            (hash-set! π v u)
            (VISIT v)))
        (hash-set! color u 'black)
        (set! time (add1 time))
        (set! tsorted (cons u tsorted))
        (hash-set! f u time))))
  
  (values color d f π tsorted)
  )

;; Performs depth-first search on graph g
;; algorithm from p541 CLRS2e
;; do extra sorting on order of vertices
;; args are the same args to sort fn
(define (dfs-with-sorting g lt #:key [extract-key (λ (x) x)] #:cache-keys? [cache-keys? #f])
  (define color (make-hash))
  ;; d and f map vertices to timestamps
  (define d (make-hash)) ;; d[v] = when vertex v discovered (v gray)
  (define f (make-hash)) ;; f[v] = when search finishes v's adj list (v black)
  (define π (make-hash))
  (define time 0)
  (define tsorted null)
  (define vertices (sort (hash-keys g) lt #:key extract-key #:cache-keys? cache-keys?))
  ;; init
  (for ([u vertices])
    (hash-set! color u 'white)
    (hash-set! π u #f))
  
  ;; do search
  (for ([u vertices])
    (when (eq? (hash-ref color u) 'white)
      ;; visit vertex u
      (let VISIT ([u u])
        (hash-set! color u 'gray)
        (set! time (add1 time))
        (hash-set! d u time)
        (for ([v (sort (set->list (hash-ref g u)) lt #:key extract-key #:cache-keys? cache-keys?)])
          (when (eq? (hash-ref color v) 'white)
            (hash-set! π v u)
            (VISIT v)))
        (hash-set! color u 'black)
        (set! time (add1 time))
        (set! tsorted (cons u tsorted))
        (hash-set! f u time))))
  
  (values color d f π tsorted)
  )

;; true if there is a path from u to v in g
#;(define (path? g u v)
  (let LOOP ([u u] [seen (set u)])
    (or (equal? u v)
        (for/or ([x (in-set (hash-ref g u))])
          (and (not (set-member? seen x))
               (LOOP x (set-add seen x)))))))

;; true if g is a directed acyclic graph (dag)
;; (this is mostly copied from dfs -- without some of the result sets)
(define (dag? g)
  (define color (make-hash))
  ;; d and f map vertices to timestamps
;  (define d (make-hash)) ;; d[v] = when vertex v discovered (v gray)
;  (define f (make-hash)) ;; f[v] = when search finishes v's adj list (v black)
;  (define π (make-hash))
;  (define time 0)
;  (define tsorted null)
  (define vertices (hash-keys g))
  ;; init
  (for ([u vertices])
    (hash-set! color u 'white)
    #;(hash-set! π u #f))
  
  ;; do search
  (for/and ([u vertices])
    (if (eq? (hash-ref color u) 'white)
      ;; visit vertex u
      (let VISIT ([u u])
        (hash-set! color u 'gray)
;        (set! time (add1 time))
;        (hash-set! d u time)
        (begin0
        (for/and ([v (in-set (hash-ref g u))])
          (cond [(eq? (hash-ref color v) 'white) ; white = tree edge = ok
;                (hash-set! π v u)
                 (VISIT v)]
                [(eq? (hash-ref color v) 'gray) #f] ; gray = back edge = cycle
                [else #t]
                ))
        (hash-set! color u 'black))
;        (set! time (add1 time))
;        (set! tsorted (cons u tsorted))
        #;(hash-set! f u time))
      #t)) ; if not white, then there are vacuously no cycles from this node
  
;  (values color d f π tsorted)
  )
  
;; result is invalid if g is not dag
(define (tsort g [dfs dfs]) (match/values (dfs g) [(_ _ _ _ sorted) sorted]))