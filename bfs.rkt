#lang racket
(require "graph-defs.rkt")
(provide bfs)

(require data/queue)

(define (bfs-generic g src #:init-queue [Q (make-queue)]
                           #:enqueue [enqueue enqueue!]
                           #:dequeue [dequeue dequeue!]
                           #:queue-empty? [queue-empty? queue-empty?]
                           #:unvisited? [unvisited? (λ (x) #t)]
                           #:pre-visit [pre-visit (λ (u) (void))]
                           #:visit [visit (λ (u v) (void))]
                           #:post-visit [post-visit (λ (u) (void))])
  (do () ((queue-empty? Q))
    (let ([u (dequeue Q)])
      (pre-visit u)
      (for ([v (in-neighbors g u)])
        (when (unvisited? v)
          (visit u v)
          (enqueue Q v)))
      (post-visit u))))


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
  (for ([u (in-vertices g)])
    (if (equal? s u)
        (begin
          (hash-set! color u 'gray)
          (hash-set! dist u 0))
        (begin
          (hash-set! color u 'white)
          (hash-set! dist u +inf.0)))
    (hash-set! π u #f))
  (enqueue! Q s)
  
  (define (white? v) (eq? 'white (hash-ref color v)))
  (define (visit u v)
    (hash-set! color v 'gray)
    (hash-set! dist v (add1 (hash-ref dist u)))
    (hash-set! π v u))
  (define (post-visit u) (hash-set! color u 'black))

  (bfs-generic g s #:init-queue Q
                   #:unvisited? white?
                   #:visit visit
                   #:post-visit post-visit)
  (values color dist π)
                   
  ;; do search
  #;(do () ((queue-empty? Q) (values color dist π))
    (let ([u (dequeue! Q)])
      (for ([v (in-set (hash-ref g u))])
        (when (eq? 'white (hash-ref color v))
          (hash-set! color v 'gray)
          (hash-set! dist v (add1 (hash-ref dist u)))
          (hash-set! π v u)
          (enqueue! Q v)))
      (hash-set! color u 'black))))
