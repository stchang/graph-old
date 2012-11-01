#lang racket
(require "graph-defs.rkt")
(provide bfs bfs-generic)

;(require data/queue)
(require "queue-generic.rkt")


(define (bfs-generic G src Q #:visited? [visited? (λ (u) #t)]
                             #:pre-visit [pre-visit (λ (u) (void))]
                             #:visit [visit (λ (u v) (void))]
                             #:process-visited [process-visited (λ (u) (void))]
                             #:post-visit [post-visit (λ (u) (void))])
  (do () ((queue-empty? Q))
    (let ([u (dequeue! Q)])
      (pre-visit u)
      (for ([v (in-neighbors G u)])
        (if (visited? v) 
            (visit u v)
            (process-visited v)))
      (post-visit u))))


;; Performs breadth-first search on graph g and source s
;; bfs : Graph Node -> (values [HashOf (Node -> Color)]
;;                             [HashOf (Node -> Number)]
;;                             [HashOf (Node -> Node)])
;; algorithm from p532 CLRS2e
(define (bfs G s)
  ;; color maps vertices to 'white or 'gray or 'black
  ;;   white = unseen
  ;;   black = seen and done checking neighbors
  ;;   gray = seen but have not checked neighbors (in the queue)
  (define color (make-hash))
  (define (white? v) (eq? 'white (hash-ref color v)))
  (define dist (make-hash))
  (define π (make-hash))
  (define Q (make-queue))

  ;; init
  (for ([u (in-vertices G)])
    (hash-set! color u 'white)
    (hash-set! dist u +inf.0)
    (hash-set! π u #f))
  (hash-set! color s 'gray)
  (hash-set! dist s 0)
  (enqueue! Q s)
  
  (define (visit u v)
    (hash-set! color v 'gray)
    (hash-set! dist v (add1 (hash-ref dist u)))
    (hash-set! π v u)
    (enqueue! Q v))
  (define (post-visit u) (hash-set! color u 'black))

  (bfs-generic G s Q #:visited? white?
                     #:visit visit
                     #:post-visit post-visit)
  
  (values dist π))