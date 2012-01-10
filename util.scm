(use-modules (srfi srfi-27))

(define random-source (make-random-source))
(random-source-randomize! random-source)

(define rand-int (random-source-make-integers random-source))
(define rand-float (random-source-make-reals random-source))

(define list->pair (lambda (l) (cons (car l) (cadr l))))
