#lang racket
(require racket/generic)

(require (prefix-in q: data/queue) 
         (prefix-in h: data/heap))

(provide make-queue make-heap
         queue-empty? enqueue! dequeue!)

(define-generics queue
  (queue-empty? queue)
  (enqueue! queue x)
  (dequeue! queue))

;; (make-Q queue)
(define-struct Q (q)
  #:methods gen:queue
  [(define (queue-empty? q) (q:queue-empty? (Q-q q)))
   (define (enqueue! q x) (q:enqueue! (Q-q q) x))
   (define (dequeue! q) (q:dequeue! (Q-q q)))])

(define (make-queue) (Q (q:make-queue)))

;; (make-H heap)
(define-struct H (h)
  #:methods gen:queue
  [(define (queue-empty? q) (<= (h:heap-count (H-h q)) 0))
   (define (enqueue! q x) (h:heap-add! (H-h q) x))
   (define (dequeue! q) 
     (define hp (H-h q))
     (define min (h:heap-min hp))
     (h:heap-remove-min! hp)
     min)])

(define (make-heap lt) (H (make-heap lt)))