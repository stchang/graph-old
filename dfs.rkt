#lang racket
(require "graph-defs.rkt")
(provide dfs)

(define (dfs-generic g #:unvisited? [unvisited? (λ (x) #t)]) g)
                       
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