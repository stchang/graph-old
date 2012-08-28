#lang racket
(require (only-in "graph.rkt" bfs))

;; algorithms to calculuate Minimum Spanning Tree (MST) of 
;; connected, undirected graph

;; Kruskal: greedy algorithm to calculate MST 
;; O(E log V)

;; Prim: greedy algorithm to calculate MST
;; O(E log V) w binary heaps
;; O(E + V log V) w Fibonacci heaps