#lang racket

(require data/queue)

(provide graph weighted-graph make-graph 
         add-edge add-di-edge add-edge! add-di-edge! add-vertex add-vertex!
         get-neighbors get-vertices
         bfs dfs dfs-with-sorting tsort dag? tsorted? transpose scc print-π)

;; TODO:
;; 2012-07-15
;; o) wrap hash table in graph struct
;; o) add grapheq and grapheqv versions corresponding to hasheq and hasheqv
;; 2012-07-28
;; x) add dag? pred - DONE 2012-08-12
;; x) add tsorted? pred - DONE 2012-08-17
;; x) add graph-fold-dfs - DONE 2012-08-17
;; 2012-08-12
;; o) generalize dfs and bfs
;;    - for example, dag? has code copied from dfs
;; 2012-08-17
;; o) abstract dfs, dfs-with-sorting, graph-fold-dfs, dag?, etc
;;    to get rid of code duplication
;; o) impl path? with bfs
;; o) finish impl scc -- extract sccs from π2
;; 2012-08-25
;; o) add dfs-single, which starts from one node, and only traverses
;;    nodes connected to that node
;; o) graph-ormap-dfs should be graph-ormap-dfs-single

; 2012-07-15: not used -- graph is currently a hash table
#;(define-struct graph (ht)
  #:omit-define-syntaxes)

(define (get-neighbors g v) (hash-ref g v))
(define (get-vertices g) (hash-keys g))


; graph is a immutable hash table -- in the future wrap the ht with a struct
(define-syntax (graph stx)
  (syntax-case stx (-- -> <-)
    [(_) #'(make-immutable-hash)]
    [(_ v rest ...) (identifier? #'v) #'(add-vertex (graph rest ...) 'v)]
    [(_ (u -- v) rest ...) #'(add-edge    (graph rest ...) 'u 'v)]
    [(_ (u -> v) rest ...) #'(add-di-edge (graph rest ...) 'u 'v)]
    [(_ (u <- v) rest ...) #'(add-di-edge (graph rest ...) 'v 'u)]
    [(_ ((u (v ...)) ...)) 
     #'(make-immutable-hash (list (cons 'u (apply set '(v ...))) ...))]
    [(_ assocs) #'(make-immutable-hash assocs)]))

; weighted-graph (copied from graph): uses immutable hash table
(define-syntax (weighted-graph stx)
  (define (get-undirected-edge-weight e)
    (regexp-match 
      (regexp "(?<=^--)[0-9]+(?=--$)")
      (symbol->string (syntax->datum e))))
  (syntax-case stx (-- -> <-)
    [(_) #'(make-immutable-hash)]
    [(_ v rest ...) (identifier? #'v) #'(add-vertex (graph rest ...) 'v)]
    [(_ (u --w-- v) rest ...)
     (get-undirected-edge-weight #'--w--)
     (let ([w (regexp-match 
               (regexp "(?<=^--)[0-9]+(?=--$)")
               (symbol->string (syntax->datum #'--w--)))])
       (with-syntax ([(w) (datum->syntax #'--w-- w)])
       #'(add-weighted-edge (weighted-graph rest ...) 'u 'v w)))]
    [(_ (u -> v) rest ...) #'(add-di-edge (graph rest ...) 'u 'v)]
    [(_ (u <- v) rest ...) #'(add-di-edge (graph rest ...) 'v 'u)]
    [(_ ((u (v ...)) ...)) 
     #'(make-immutable-hash (list (cons 'u (apply set '(v ...))) ...))]
    [(_ assocs) #'(make-immutable-hash assocs)]))

; make graph with mutable hash table
(define-syntax (make-graph stx)
  (syntax-case stx (-- -> <-)
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

(define (add-weighted-di-edge g u v w)
  (add-vertex (add-weighted-di-edge-no-vertex-check g u v w) v))
(define (add-weighted-di-edge-no-vertex-check g u v w)
  (hash-update g u (λ (s) (set-add s (cons v w))) (set)))
(define (add-weighted-edge g u v w)
  (add-weighted-di-edge-no-vertex-check (add-weighted-di-edge g u v w) v u w))

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
  ;; white = unseen
  ;; black = seen and done checking neighbors
  ;; gray = seen but have not checked neighbors (in the queue)
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
;  (define tsorted null)
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
;        (set! tsorted (cons u tsorted))
        (hash-set! f u time))))
  
  (values color d f π)
  )

;; graph-fold-dfs
;; folds over a graph, where the vertices are traversed in dfs order
;; code copied from dfs
(define (graph-fold-dfs g f base)
  (define color (make-hash))
  ;; d and f map vertices to timestamps
;  (define d (make-hash)) ;; d[v] = when vertex v discovered (v gray)
;  (define f (make-hash)) ;; f[v] = when search finishes v's adj list (v black)
;  (define π (make-hash))
;  (define time 0)
  (define result base)
  (define vertices (get-vertices g))
  ;; init
  (for ([u vertices])
    (hash-set! color u 'white)
;    (hash-set! π u #f)
    )
  
  ;; do search
  (for ([u vertices])
    (when (eq? (hash-ref color u) 'white)
      ;; visit vertex u
      (let VISIT ([u u])
        (hash-set! color u 'gray)
;        (set! time (add1 time))
;        (hash-set! d u time)
        (for ([v (in-set (hash-ref g u))])
          (when (eq? (hash-ref color v) 'white)
;            (hash-set! π v u)
            (VISIT v)))
        (hash-set! color u 'black)
;        (set! time (add1 time))
        (set! result (f u result))
;        (hash-set! f u time)
        )))
  
  result
  )

;; graph-fold-dfs-with-sorted
;; folds over a graph, where the vertices are traversed in dfs order
;; when doing dfs, consider vertices in given sorted order
;; code copied from graph-fold-dfs
(define (graph-fold-dfs-with-sorted 
         g sorted-vs 
;         visit-start-fn visit-start-base ; during visit, before recur
         visit-end-fn visit-end-base ; during visit, after recur
         [post-visit-fn (λ (x y) y)] [post-visit-base #f]) ; after visit
                                      
  (define color (make-hash))
  ;; d and f map vertices to timestamps
;  (define d (make-hash)) ;; d[v] = when vertex v discovered (v gray)
;  (define f (make-hash)) ;; f[v] = when search finishes v's adj list (v black)
;  (define π (make-hash))
;  (define time 0)
  (define visit-end-result visit-end-base)
  (define result post-visit-base)
  ;; init
  (for ([u sorted-vs])
    (hash-set! color u 'white)
;    (hash-set! π u #f)
    )
  
  ;; do search
  (for ([u sorted-vs])
    (when (eq? (hash-ref color u) 'white)
      ;; visit vertex u
      (let VISIT ([u u])
        (hash-set! color u 'gray)
;        (set! time (add1 time))
;        (hash-set! d u time)
        (for ([v (in-set (hash-ref g u))])
          (when (eq? (hash-ref color v) 'white)
;            (hash-set! π v u)
            (VISIT v)))
        (hash-set! color u 'black)
;        (set! time (add1 time))
        (set! visit-end-result (visit-end-fn u visit-end-result))
;        (hash-set! f u time)
        )
      (set! result (post-visit-fn visit-end-result result))
      (set! visit-end-result visit-end-base)
      ))
  (if post-visit-base result visit-end-result)
  )

;; traverses g in dfs order, starting with s
;; applies predicate p? to each node, stopping at first true result
;; returns true at least one true is found, false otherwise
;; (code copied from dfs, but really there should be a dfs-single that only
;;  starts from a single node and traverses connected nodes)
(define (graph-ormap-dfs g p? s)
  (define color (make-hash))
  ;; d and f map vertices to timestamps
;  (define d (make-hash)) ;; d[v] = when vertex v discovered (v gray)
;  (define f (make-hash)) ;; f[v] = when search finishes v's adj list (v black)
;  (define π (make-hash))
;  (define time 0)
  (define vertices (hash-keys g))
  ;; init
  (for ([u vertices])
    (hash-set! color u 'white)
;    (hash-set! π u #f)
    )
  
  (let VISIT ([u s])
    (hash-set! color u 'gray)
;        (set! time (add1 time))
;        (hash-set! d u time)
    (for/or ([v (in-set (hash-ref g u))])
      (or (p? v)
          (and (eq? (hash-ref color v) 'white)
;            (hash-set! π v u)
               (VISIT v)))))
;    (hash-set! color u 'black)
;        (set! time (add1 time))
;        (hash-set! f u time)
    )

;; should be able to use dfs to impl
;; true if there is a path from u to v in g
(define (path? g u v)
  (graph-ormap-dfs g (λ (x) (equal? x v)) u)
  #;(let LOOP ([u u] [seen (set u)])
    (or (equal? u v)
        (for/or ([x (in-set (hash-ref g u))])
          (and (not (set-member? seen x))
               (LOOP x (set-add seen x)))))))

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
;  (define tsorted null)
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
;        (set! tsorted (cons u tsorted))
        (hash-set! f u time))))
  
  (values color d f π)
  )

(define (tails lst)
  (if (null? lst)
      (list null)
      (cons lst (tails (rest lst)))))
;; returns true if sorted-vs is a list of vertices in g in tsorted order
;; assumes g is a dag
(define (tsorted? g sorted-vs)
    (if (null? sorted-vs)
      true
      (and (let ([u (first sorted-vs)])
             (for/and ([v (rest sorted-vs)])
               (not (path? g v u))))
           (tsorted? g (rest sorted-vs)))))

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
;;(define (tsort g [dfs dfs]) (match/values (dfs g) [(_ _ _ _ sorted) sorted]))
(define (tsort g [dfs graph-fold-dfs]) (dfs g cons null))

;; if g = (V,E), then (transpose g) = g' = (V,E'), where E' = {(u,v)|(v,u)∈E}
;; running time O(V+E)
(define (transpose g)
  (cond [(immutable? g)
         (for/fold ([new-g (graph)]) ([(u vs) (in-hash g)])
           (for/fold ([new-g (add-vertex new-g u)]) ([v vs])
             (add-di-edge-no-vertex-check new-g v u)))]
        [else
         (define new-g (make-graph))
         (for ([(u vs) (in-hash g)])
           (add-vertex! new-g u)
           (for ([v vs])
             (add-di-edge!-no-vertex-check new-g v u)))]))

(define (scc g)
  (define f (graph-fold-dfs g cons null))
  (graph-fold-dfs-with-sorted (transpose g) f cons null cons null)
  )


(define (print-π π s)
  (printf "~a " s)
  (let ([next (hash-ref π s)])
    (when next (print-π π next))))