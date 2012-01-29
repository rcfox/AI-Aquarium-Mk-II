(define-module (tree quadtree)
  #:use-module (srfi srfi-1)
  #:export (quadtree
			make-quadtree
			quadtree-insert! quadtree-remove! quadtree-search quadtree-get-region))

(define quadtree (make-record-type 'quadtree '(position size children)))
(define make-quadtree (lambda* (#:optional #:key (size 1) (pos '(0 . 0)))
							   ((record-constructor quadtree) pos size '())))

(define position (lambda (tree)
				   ((record-accessor quadtree 'position) tree)))

(define size (lambda (tree)
				   ((record-accessor quadtree 'size) tree)))

(define children (lambda (tree)
				   ((record-accessor quadtree 'children) tree)))

(define set-children! (lambda (tree value)
						((record-modifier quadtree 'children) tree value)))

(define p+p (lambda (a b)
			  (cons (+ (car a) (car b)) (+ (cdr a) (cdr b)))))

(define p+n (lambda (a b)
			  (cons (+ (car a) b) (+ (cdr a) b))))

(define between? (lambda (top-left bottom-right pos)
				   (and (and (>= (car pos) (car top-left))
							 (>= (cdr pos) (cdr top-left)))
						(and (< (car pos) (car bottom-right))
							 (< (cdr pos) (cdr bottom-right))))))

(define do-at-position (lambda (tree pos proc)
						 (let ((tree-position (position tree)))
						   (if (between? tree-position (p+n tree-position (size tree)) pos)
							   (if (= (size tree) 1)
								   (proc tree)
								   (begin
									 (if (null? (children tree))
										 (let ((s (/ (size tree) 2)))
										   (set-children! tree (list (make-quadtree #:pos tree-position #:size s)
																	 (make-quadtree #:pos (p+p tree-position (cons s 0)) #:size s)
																	 (make-quadtree #:pos (p+p tree-position (cons 0 s)) #:size s)
																	 (make-quadtree #:pos (p+n tree-position s) #:size s)))))
									 (append-map (lambda (t) (do-at-position t pos proc)) (children tree))))
							   '()))))

(define quadtree-get-region (lambda (tree top-left bottom-right)
							  (let ((next (call/cc (lambda (return)
													 (for-each (lambda (child)
																 (let* ((tl (position child))
																		(br (p+n tl (size child))))
																   (if (and (between? tl br top-left) (between? tl br bottom-right))
																	   (return child))))
															   (children tree))
													 '()))))
								(if (null? next)
									tree
									(quadtree-get-region next top-left bottom-right)))))

(define quadtree-insert! (lambda (tree pos item)
				  (do-at-position tree pos (lambda (tree)
											 (set-children! tree (cons item (children tree)))
											 (children tree)))))

(define quadtree-remove! (lambda (tree pos item)
				  (do-at-position tree pos (lambda (tree)
											 (set-children! tree (delete item (children tree)))
											 (children tree)))))

(define quadtree-search (lambda (tree pos)
				 (do-at-position tree pos (lambda (tree)
											(children tree)))))
(define test (lambda ()
			   (let ((tree (make-quadtree #:size 32)))
				 (quadtree-insert! tree '(0 . 0) 'a)
				 (quadtree-insert! tree '(1 . 0) 'b)
				 (quadtree-insert! tree '(1 . 1) 'c)
				 (quadtree-insert! tree '(3 . 8) 'd)
				 (quadtree-insert! tree '(13 . 5) 'e)
				 (quadtree-insert! tree '(7 . 7) 'f)
				 tree)))
