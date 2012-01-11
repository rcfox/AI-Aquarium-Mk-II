(use-modules (srfi srfi-26)
			 (srfi srfi-27))

(define random-source (make-random-source))
(random-source-randomize! random-source)

(define rand-int (random-source-make-integers random-source))
(define rand-float (random-source-make-reals random-source))

(define list->pair (lambda (l) (cons (car l) (cadr l))))

;; Stolen from cky
(define (cartesian-product first . rest)
  (define (iter l result)
    (define (prepend-all x)
      (map (cut cons <> x) l))
    (concatenate (map prepend-all result)))
  (map reverse (fold iter (map list first) rest)))

(define (random-element list)
  (list-ref list (rand-int (length list))))
