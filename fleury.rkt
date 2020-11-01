#lang racket

(require graph
         pfds/heap/leftist
         racket/match)
(provide color)

(define *edges*
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
    (hash-keys *edges*)))

(define-edge-property *g* color)
(for ([(edge col) (in-hash *edges*)])
  (color-set! (car edge) (cadr edge) col))

(define (remove-edge G u v)
  (let ([new-graph (graph-copy G)])
    (remove-edge! new-graph u v)
    new-graph))

(struct maybe-step (vertex next-graph ccs))
(define/match (candidate-comparator x y)
  [((maybe-step _ _ xcs) (maybe-step _ _ ycs)) (< (length xcs) (length ycs))])

(define (edge-candidates G start)
  (for/fold ([acc (heap candidate-comparator)])
            ([v (in-neighbors G start)])
    (let ([without-edge (remove-edge G start v)])
      (insert (maybe-step v without-edge (cc without-edge)) acc))))

(define (traverse G start)
  (if (zero? (length (get-edges G)))
      '()
      (let ([next-step (find-min/max (edge-candidates G start))])
        (cons (list start (maybe-step-vertex next-step))
              (traverse (maybe-step-next-graph next-step) (maybe-step-vertex next-step))))))

(traverse *g* 'c)
