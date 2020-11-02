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

(struct step (vertex graph ccs))

(define/match (candidate-comparator x y)
  [((step _ _ xcs) (step _ _ ycs)) (< (length xcs) (length ycs))])

(define (make-step G start v)
  (let ([without-edge (remove-edge G start v)])
    (step v without-edge (cc without-edge))))

(define (edge-candidates G start)
  (for/fold ([acc (heap candidate-comparator)])
            ([s (sequence-map (curry make-step G start) (in-neighbors G start))]
             #:final (= 1 (length (step-ccs s))))
    (insert s acc)))

(define (traverse G start)
  (if (zero? (length (get-edges G)))
      '()
      (let ([next (find-min/max (edge-candidates G start))])
        (cons (list start (step-vertex next))
              (traverse (step-graph next) (step-vertex next))))))

(traverse *g* 'c)
