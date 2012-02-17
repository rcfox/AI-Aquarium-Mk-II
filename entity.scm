(use-modules (oop goops))

(define-class <entity> ()
  (position #:accessor position #:init-keyword #:pos)
  (name #:init-value "Entity" #:accessor name #:init-keyword #:name)
  (appearance #:init-value (make <map-element> #:r #\? #:f '(255 255 255)) #:accessor appearance #:init-keyword #:appearance)
  (walkable #:init-value #t #:accessor walkable #:init-keyword #:walkable)
  (transparent #:init-value #t #:accessor transparent #:init-keyword #:transparent))

(define-method (add! (m <map>) (e <entity>))
  (set! (entities m) (cons e (entities m)))
  (quadtree-insert! (quadtree m) (position e) e)
  (set-properties! m (position e)))

(define-method (rem! (m <map>) (e <entity>))
  (quadtree-remove! (quadtree m) (position e) e)
  (set! (entities m) (delq e (entities m)))
  (set-properties! m (position e)))

(define-method (die (e <entity>))
  (rem! m e))

(define-method (distance (e1 <entity>) (e2 <entity>))
  (distance (position e1) (position e2)))

(define-method (distance (p1 <pair>) (p2 <pair>))
  (sqrt (+ (expt (- (car p1) (car p2)) 2) (expt (- (cdr p1) (cdr p2)) 2))))

(define-method (move! (e <entity>) (p <pair>))
  (if (not (eq? (position e) p))
	  (let ((region (quadtree-get-region (quadtree m) (position e) p)))
		(quadtree-remove! region (position e) e)
		(set-properties! m (position e))
		(set! (position e) p)
		(quadtree-insert! region p e)
		(set-properties! m (position e)))))

(define-class <can-move> (<entity>)
  (moved-hook #:init-form (make-hook 1) #:accessor moved-hook)
  (path #:accessor path)
  (destination #:accessor destination
			   #:allocation #:virtual
			   #:slot-ref (lambda (e)
							(libtcod-path-destination (path e)))
			   #:slot-set! (lambda (e p)
							 (let* ((point (get-data m p))
									(can-walk (walkable point))
									(transp (transparent point)))
							   ;; Temporarily set the square as walkable so that the pathfinder won't give up immediately.
							   (libtcod-map-set! (libtcod-data (seen-map e)) (car p) (cdr p) #t #t)
							   (libtcod-path-compute (path e) (position e) p)
							   (libtcod-map-set! (libtcod-data (seen-map e)) (car p) (cdr p) transp can-walk)))))

(define-method (add! (m <map>) (e <can-move>))
  (set! (path e) (make-libtcod-path (libtcod-data m)))
  (next-method))

(define-method (move! (e <can-move>) (p <pair>))
  (next-method)
  (run-hook (moved-hook e) e))

(define-method (walk-path (e <can-move>))
  (libtcod-path-walk (path e) #t))

(define-class <can-see> (<entity>)
  (sight-radius #:accessor sight-radius #:init-value 5 #:init-keyword #:sight)
  (seen-space #:accessor seen-space #:init-value '())
  (seen-entities #:accessor seen-entities #:init-value '())
  (seen-map #:accessor seen-map))

(define-method (initialize (e <can-see>) initargs)
  (next-method)
  (set! (seen-map e) (make <map> #:w (width m) #:h (height m))))

(define-method (add! (m <map>) (e <can-see>))
  (if (is-a? e <can-move>)
	  (set! (path e) (make-libtcod-path (libtcod-data (seen-map e)))))
  (next-method))

(define-method (look (e <can-see>))
  (set! (seen-space e) (fov m (position e) (sight-radius e)))
  (for-each (lambda (x)
			  (set-data! (seen-map e) x (get-data m x)))
			(seen-space e))
  (let* ((top-left (cons (- (car (position e)) (sight-radius e)) (- (cdr (position e)) (sight-radius e))))
		 (bottom-right (cons (+ (car (position e)) (sight-radius e)) (+ (cdr (position e)) (sight-radius e))))
		 (region (quadtree-get-region (quadtree m) top-left bottom-right)))
	(set! (seen-entities e) (append-map (lambda (x)
										  (quadtree-search region x))
										(seen-space e))))
  (seen-space e))

(define-class <living> (<entity>)
  (health #:init-value 100 #:accessor health #:init-keyword #:health)
  (walkable #:init-value #f #:accessor walkable #:init-keyword #:walkable)
  (transparent #:init-value #t #:accessor transparent #:init-keyword #:transparent))

(define-method (damage (e <living>) points)
  (set! (health e) (- (health e) points))
  (if (<= (health e) 0)
	  (begin
		(die e)
		#t)
	  #f))

(define-class <has-goals> (<entity>)
  (goals #:init-value '() #:accessor goals))

(define-class <has-inventory> (<entity>)
  (inventory #:init-value '() #:accessor inventory))

(define-method (die (e <has-inventory>))
  (for-each (lambda (i)
			  (add! m i)
			  (rem! e i)
			  (move! i (random-element (filter (lambda (p) (walkable (get-data m p))) (seen-space e)))))
			(inventory e))
  (next-method))

(define-method (add! (e <has-inventory>) (i <entity>))
  (set! (inventory e) (cons i (inventory e))))

(define-method (rem! (e <has-inventory>) (i <entity>))
  (set! (inventory e) (delq i (inventory e))))

(define-class <good-guy> (<entity>))
(define-class <bad-guy> (<entity>))

(define-class <person> (<living> <can-move> <can-see> <has-goals> <has-inventory> <good-guy>)
  (appearance #:init-value (make <map-element> #:r #\@ #:f '(0 0 255)) #:accessor appearance #:init-keyword #:appearance))

(define-class <monster> (<living> <can-move> <can-see> <has-goals> <has-inventory> <bad-guy>)
  (appearance #:init-value (make <map-element> #:r #\M #:f '(255 0 0)) #:accessor appearance #:init-keyword #:appearance))
