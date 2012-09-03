#lang racket

(require data/heap)

(provide in-heap)

(define-syntax (in-heap stx)
  (syntax-case stx ()
    [(_ h)
     ;; position = heap h
     #'(make-do-sequence 
        (thunk (values 
                heap-min
                (λ (hp) (heap-remove-min! hp) hp)
                h
                (λ (hp) (not (= 0 (heap-count hp))))
                #f
                #f)))]))