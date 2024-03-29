#lang scribble/manual
@(require scribble/eval
          (for-label "../graph-defs.rkt"
                     racket/contract
                     racket/base))

@title{Graphs}

@(define the-eval (make-base-eval))
@(the-eval '(require racket/set))
@(the-eval '(require "../graph-defs.rkt"))

@defmodule["../graph-defs.rkt"]

@author[@author+email["Stephen Chang" "stchang@racket-lang.org"]]

Graph library. Partially based on C++ Boost Graph Library. See Lee et al. OOPSLA 1999 @cite["GGCL"].

@defthing[gen:graph any/c]{
  A @tech{generic interface} (see @secref["struct-generics"]) that defines a @deftech{graph}. To supply method implementations, a struct should use the @racket[#:methods] form. A @tech{graph} has the following methods:

@itemize[
 
  @item{@racket[add-vertex]: Accepts two arguments, a graph and a vertex. 
         Adds the vertex to the graph.}
  @item{@racket[add-edge]: Accepts three or four arguments, a graph, two vertices, and an optional weight value. Adds the undirected edge comprised of the two vertices and the optional weight to the graph, adding the vertices first if necessary. Equivalent to adding two directed edges, one in each direction.}
  @item{@racket[add-di-edge]: Accepts three or four arguments, a graph, source and destination vertices, and an optional weight value. Adds to the graph the directed, optionally weighted edge going from the source to the destination.}
  @item{@racket[in-vertices]: Accepts one argument, a graph. Returns a sequence whose elements are the vertices of the graph.}
  @item{@racket[in-neighbors]: Accepts two arguments, a graph and a vertex. Returns a sequence whose elements are the vertex's neighbors in the graph.}
  @item{@racket[in-weighted-neighbors]: Accepts two arguments, a graph and a vertex. Returns a sequence whose elements consist of two values, a neighbor of the vertex and the weight of the edge connecting the vertex to that neighbor.}
          
]
}

@defproc[(graph? [g any/c]) boolean?]{
  Returns @racket[#t] if @racket[g] is a @tech{graph} and @racket[#f] otherwise.
}


@defform/subs[(graph edge-or-vertex ...)
              ([edge-or-vertex vertex
                               (vertex edge vertex)]
               [vertex datum]
               [edge -- → ←])]{
  Syntax to create an unweighted graph using literals. Vertices can be any literal. 
  Edges are specified with two vertices, separated by either @racket[--], for undirected, 
  or @racket[→] or @racket[←], for directed edges 
  (the literal symbols are \rightarrow and \leftarrow).
  
  @examples[
  #:eval the-eval
  (graph a b c)
  (graph (a -- b) (a -- c) d)
  (graph a (b → c) (c → d) (d → a))
  (graph (a ← b) c (c ← d) (d → e) f)
  (graph (1 → 2) 3 (1 → a) (b ← 4) c (a -- b))
  ]
}
                              
@defform/subs[(graph ((vertex (vertex ...)) ...))
              ([vertex datum])]{
  Alternative syntax to create unweighted graph using vertex/list-of-neighbor-vertices 
  pairs of literals. A vertex is any literal.
  
  @examples[
  #:eval the-eval
  (graph (a (a b c)) (b (c d e)) (f (d)))
  (graph (1 (2 3)) (4 (a b)))
  ]
}

@defform[(graph assocs)]{
  Creates an unweighted graph using list of vertex/set-of-neighbor pair values 
  (as opposed to literals). A vertex is any value and the neighbors must be a set.
  
  @examples[
  #:eval the-eval
  (graph (list (cons 'a (set 'b 'c)) (cons 'b (set 'c 'd))))
  ]
}



@defproc[(add-vertex [graph graph?]
                     [v any/c])
         graph?]{
  Adds vertex @racket[v] to @racket[graph] and returns the new graph. 
  Vertices are compared using @racket[equal?].
}
                
@defproc[(add-edge [graph graph?]
                   [u any/c]
                   [v any/c]
                   [weight 1])
         graph?]{
  Adds the undirected edge from @racket[u] to @racket[v], with optional 
  @racket[weight] to @racket[graph], adding the vertices first, if necessary.
  Equivalent to adding two directed edges, one in each direction.
}
                
@defproc[(add-di-edge [graph graph?]
                      [u any/c]
                      [v any/c]
                      [weight 1]) 
         graph?]{
  Adds the directed edge from @racket[u] to @racket[v], with optional 
  @racket[weight] to @racket[graph], adding the vertices first, if necessary. 
}

@defproc[(in-vertices [graph graph?])
         sequence?]{              
  Returns a sequence whose elements are the vertices of @racket[graph].
}

@defproc[(in-neighbors [graph graph?]
                       [v any/c])
         sequence?]{
  Returns a sequence whose elements are the neighbors of vertex @racket[v] 
  in @racket[graph].                                                            
}

@defproc[(in-weighted-neighbors [graph graph?]
                                [v any/c])
         sequence?]{
  Returns a sequence whose elements consist of two values, a neighbor of
  vertex @racket[v] in @racket[graph], and the weight of the edge connecting 
  @racket[v] to that neighbor.
}



@(bibliography
  
  (bib-entry #:key "GGCL"
             #:author "Lie-Quan Lee, Jeremy G. Siek, and Andrew Lumsdaine"
             #:title "The Generic Graph Component Library"
             #:location "OOPSLA"
             #:date "1999"))