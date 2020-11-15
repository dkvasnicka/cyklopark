#lang racket

(require graph)
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

(struct step (vertex graph ccs) #:transparent)

(define (make-step G start v)
  (let ([without-edge (remove-edge G start v)])
    (step v without-edge (length (cc without-edge)))))

(define (next-edge G start cc-count)
  (let ([potential-steps (stream-map (curry make-step G start)
                                     (sequence->stream (in-neighbors G start)))])
    (or (for/first ([s potential-steps] #:when (= cc-count (step-ccs s)))
          s)
        (stream-first potential-steps))))

(define (traverse stp)
  (if (zero? (length (get-edges (step-graph stp))))
      '()
      (let ([next (next-edge (step-graph stp) (step-vertex stp) (step-ccs stp))])
        (cons (list (step-vertex stp) (step-vertex next))
              (traverse next)))))

(traverse (step 'c *g* 1))
