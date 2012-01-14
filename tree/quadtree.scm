(define-module (tree quadtree)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (<quadtree>
			insert! remove! search))

(define-class <quadtree> ()
  (position #:accessor position #:init-keyword #:pos #:init-value '(0 . 0))
  (size #:accessor size #:init-keyword #:size)
  (children #:init-value '() #:accessor children))

;; Is there a better way to keep these within the scope of this module?
(define-method (quadtree:+ (a <pair>) (b <pair>))
  (cons (+ (car a) (car b)) (+ (cdr a) (cdr b))))
(define-method (quadtree:+ (a <pair>) (b <number>))
  (cons (+ (car a) b) (+ (cdr a) b)))

(define-method (between? (top-left <pair>) (bottom-right <pair>) (pos <pair>))
  (and (and (>= (car pos) (car top-left))
			(>= (cdr pos) (cdr top-left)))
	   (and (< (car pos) (car bottom-right))
			(< (cdr pos) (cdr bottom-right)))))

(define-method (do-at-position (tree <quadtree>) (pos <pair>) (proc <procedure>))
  (let ((+ quadtree:+))
	(if (between? (position tree) (+ (position tree) (size tree)) pos)
		(if (= (size tree) 1)
			(proc tree)
			(begin
			  (if (null? (children tree))
				  (let ((s (/ (size tree) 2)))
					(set! (children tree) (list (make <quadtree> #:pos (position tree) #:size s)
												(make <quadtree> #:pos (+ (position tree) (cons s 0)) #:size s)
												(make <quadtree> #:pos (+ (position tree) (cons 0 s)) #:size s)
												(make <quadtree> #:pos (+ (position tree) (cons s s)) #:size s)))))
			  (append-map (lambda (t) (do-at-position t pos proc)) (children tree))))
		'())))

(define-method (insert! (tree <quadtree>) (pos <pair>) item)
  (do-at-position tree pos (lambda (tree)
							 (set! (children tree) (cons item (children tree)))
							 (children tree))))

(define-method (remove! (tree <quadtree>) (pos <pair>) item)
  (do-at-position tree pos (lambda (tree)
							 (set! (children tree) (delete item (children tree)))
							 (children tree))))

(define-method (search (tree <quadtree>) (pos <pair>))
  (do-at-position tree pos (lambda (tree)
							 (children tree))))

(define-method (display (tree <quadtree>) port)
  (newline)
  (display "@") (display (position tree))
  (display "#") (display (size tree))
  (display " --> ")
  (for-each (lambda (c) (display c port)) (children tree)))
