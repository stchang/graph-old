#lang racket
(require racket/generic)

(require (prefix-in q: data/queue) 
         (prefix-in h: data/heap))

(provide make-queue make-heap
         queue-empty? enqueue! dequeue! refresh)

(define-generics queue
  (queue-empty? queue)
  (enqueue! queue x)
  (dequeue! queue)
  (refresh queue))

;; (make-Q queue)
(define-struct Q (q)
  #:methods gen:queue
  [(define (queue-empty? q) (q:queue-empty? (Q-q q)))
   (define (enqueue! q x) (q:enqueue! (Q-q q) x))
   (define (dequeue! q) (q:dequeue! (Q-q q)))
   (define (refresh q) (void))])

(define (make-queue) (Q (q:make-queue)))

;; (make-H heap)
(define-struct H ([h #:mutable] lt)
  #:methods gen:queue
  [(define (queue-empty? q) (<= (h:heap-count (H-h q)) 0))
   (define (enqueue! q x) (h:heap-add! (H-h q) x))
   (define (dequeue! q) 
     (define hp (H-h q))
     (define min (h:heap-min hp))
     (h:heap-remove-min! hp)
     min)
   ;; refresh re-heapifies heap
   (define (refresh q)
     (define new-hp (h:make-heap (H-lt q)))
     (h:heap-add-all! new-hp (H-h q))
     (set-H-h! q new-hp))])

(define (make-heap lt) (H (h:make-heap lt) lt))