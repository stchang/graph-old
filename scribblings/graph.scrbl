#lang scribble/manual
@(require scribble/eval
          (for-label "../graph.rkt"
                     racket/contract
                     racket/base))

@title{Graphs}

@(define the-eval (make-base-eval))
@(the-eval '(require "../graph.rkt"))

@defmodule["../graph.rkt"]

@author[@author+email["Stephen Chang" "stchang@racket-lang.org"]]

Graph library. Partially based on C++ Boost Graph Library. See Lee et al. OOPSLA 1999 @cite["GGCL"].

@defproc[(graph? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a graph and @racket[#f] otherwise.}

@defproc[(graph [assocs (listof pair?) null]) graph?]{
Creates an immutable graph, with the given key-value pairs, where a key is a vertex in the graph and a value is the set of neighbors for that vertex}


@(bibliography
  
  (bib-entry #:key "GGCL"
             #:author "Lie-Quan Lee, Jeremy G. Siek, and Andrew Lumsdaine"
             #:title "The Generic Graph Component Library"
             #:location "OOPSLA"
             #:date "1999"))