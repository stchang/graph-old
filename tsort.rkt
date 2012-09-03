#lang racket

(require "graph.rkt")

(provide tsorted? tsort)

;; returns true if sorted-vs is a list of vertices in g in tsorted order
;; assumes g is a dag
(define (tsorted? g sorted-vs)
    (if (null? sorted-vs)
      true
      (and (let ([u (first sorted-vs)])
             (for/and ([v (rest sorted-vs)])
               (not (path? g v u))))
           (tsorted? g (rest sorted-vs)))))

;; result is invalid if g is not dag
;;(define (tsort g [dfs dfs]) (match/values (dfs g) [(_ _ _ _ sorted) sorted]))
;; returns list of vertices in tsorted order
(define (tsort g [dfs graph-fold-dfs]) (dfs g cons null))