(use-modules (oop goops)
			 (srfi srfi-1))

(define-class <map-element> ()
  (representation #:init-value #\. #:accessor representation #:init-keyword #:r)
  (fore-colour #:init-value '(255 255 255) #:accessor fore-colour #:init-keyword #:f)
  (back-colour #:init-value '(0 0 0) #:accessor back-colour #:init-keyword #:b)
  (walkable #:init-value #f #:accessor walkable #:init-keyword #:walkable)
  (transparent #:init-value #f #:accessor transparent #:init-keyword #:transparent)
  (cost #:init-value 1 #:getter cost #:init-keyword #:cost))

(define <empty> (make <map-element> #:r #\ #:walkable #t #:transparent #t))
(define <wall> (make <map-element> #:r #\# #:f '(150 150 150) #:walkable #f #:transparent #f #:cost 99999))
(define <floor> (make <map-element> #:r #\. #:f '(0 50 0) #:walkable #t #:transparent #t))

(define-class <map> ()
  (w #:init-value 0 #:getter width #:init-keyword #:w)
  (h #:init-value 0 #:getter height #:init-keyword #:h)
  (data #:accessor data)
  (libtcod-data #:accessor libtcod-data)
  (entities #:init-value '() #:accessor entities))

(define-method (initialize (m <map>) initargs)
  (next-method)
  (set! (data m) (make-array <empty> (width m) (height m)))
  (set! (libtcod-data m) (make-libtcod-map (width m) (height m) #t #t)))

(define-method (set-data! (m <map>) x y (d <map-element>))
  (array-set! (data m) d x y)
  (libtcod-map-set! (libtcod-data m) x y (transparent d) (walkable d)))

(define-method (set-data! (m <map>) (p <pair>) (d <map-element>))
  (set-data! m (car p) (cdr p) d))

(define-method (get-data (m <map>) x y)
  (array-ref (data m) x y))

(define-method (get-data (m <map>) (p <pair>))
  (get-data m (car p) (cdr p)))

(define-method (for-each-map (m <map>) proc)
  (for-each-map
   m
   proc
   (iota (car (array-dimensions (data m))))
   (iota (cadr (array-dimensions (data m))))))

(define-method (for-each-map (m <map>) proc (x-list <list>) (y-list <list>))
  (for-each (lambda (y)
			  (for-each (lambda (x)
						  (if (in-bounds? m x y)
							  (proc x y (array-ref (data m) x y))))
						x-list))
			y-list))

(define-method (in-bounds? (m <map>) x y)
  (and (and (> x 0) (> y 0))
	   (and (< x (1- (width m))) (< y (1- (height m))))))

(define-method (create-box (m <map>) x y w h (t <map-element>))
  (for-each-map m (lambda (a b c) (set-data! m a b t)) (iota w x 1) (map (lambda (z) y) (iota w)))
  (for-each-map m (lambda (a b c) (set-data! m a b t)) (iota w x 1) (map (lambda (z) (1- h)) (iota w)))
  (for-each-map m (lambda (a b c) (set-data! m a b t)) (map (lambda (z) x) (iota h)) (iota h y 1))
  (for-each-map m (lambda (a b c) (set-data! m a b t)) (map (lambda (z) (1- w)) (iota h)) (iota h y 1)))

(define-method (draw (m <map>))
  (for-each-map m (lambda (x y tile)
					(draw-character x y (representation tile) (fore-colour tile) (back-colour tile)))))

(define-method (draw (m <map>) (x-list <list>) (y-list <list>) x-offset y-offset)
  (for-each-map m (lambda (x y tile)
					(draw-character (- x x-offset) (- y y-offset) (representation tile) (fore-colour tile) (back-colour tile)))
				x-list y-list))

(define-method (random-free-spot (m <map>))
  (let ((x (rand-int (width m)))
		(y (rand-int (height m))))
	(if (and (in-bounds? m x y) (walkable (get-data m x y)))
		(cons x y)
		(random-free-spot m))))

(define-method (fov (m <map>) x y radius)
  (libtcod-map-fov (libtcod-data m) x y radius #t 0))

(define-method (fov (m <map>) (p <pair>) radius)
  (fov m (car p) (cdr p) radius))

(define-class <cave-map> (<map>))

(define-method (initialize (m <cave-map>) initargs)
  (next-method)
  (randomize-map m)
  (create-box m 1 1 (1- (width m)) (1- (height m)) <wall>))

(define-method (randomize-map (m <cave-map>))
  (let ((prob 0.59))
	(for-each-map m (lambda (x y tile)
					  (if (> (rand-float) prob)
						  (begin
							(set-data! m x y <wall>))
						  (set-data! m x y <floor>)))))
  (for-each (lambda (a)
			  (for-each-map m (lambda (x y tile)
								(if (in-bounds? m x y)
									(let ((neighbours (count (lambda (x)
															   (eq? x <wall>))
															 (map (lambda (p)
																	(get-data m (+ x (car p)) (+ y (cadr p))))
																  (cartesian-product '(-1 0 1) '(-1 0 1))))))
									  (cond ((< neighbours 4)
											 (set-data! m x y <floor>))
											((> neighbours 4)
											 (set-data! m x y <wall>))))))))
			(iota 10)))
