#lang racket

(require graph)
(provide color)

(define edges
  #hasheq(((a b) . 'red)
          ((a f) . 'red)
          ((a d) . 'green)
          ((d f) . 'blue)
          ((f b) . 'blue)
          ((d e) . 'green)
          ((f e) . 'green)
          ((b e) . 'red)
          ((b c) . 'blue)
          ((e c) . 'red)
          ((d c) . 'blue)))

(define *g*
  (unweighted-graph/undirected
    (hash-keys edges)))

(define-edge-property *g* color)
(for ([(edge col) (in-hash edges)])
  (color-set! (car edge) (cadr edge) col))

(color->hash)
