(define fov-overlay (lambda (camera)
					  (lambda (x-offset y-offset)
						(for-each (lambda (e)
									((fov-overlay-single camera e) x-offset y-offset))
								  (filter (lambda (x) (is-a? x <can-see>)) (entities m))))))

(define fov-overlay-single (lambda (camera e)
					  (lambda (x-offset y-offset)
						(for-each (lambda (f)
									(let ((x (car (position f)))
										  (y (cdr (position f)))
										  (a (appearance f)))
									  (if (on-camera? camera x y)
										  (draw-character (- x x-offset) (- y y-offset) (representation a) (fore-colour a) (back-colour a)))))
								  (seen-entities e))
						(for-each (lambda (c)
									(if (on-camera? camera (car c) (cdr c))
										(set-back-colour! (- (car c) x-offset) (- (cdr c) y-offset) '(100 100 0) BKGND_ADDA
														  (expt (/ (- (sight-radius e) (distance (position e) c)) (sight-radius e)) 2))))
								  (seen-space e)))))

(define path-overlay (lambda (camera)
					   (lambda (x-offset y-offset)
						 (for-each (lambda (e)
									 (for-each (lambda (c)
												 (if (on-camera? camera (car c) (cdr c))
													 (set-back-colour! (- (car c) x-offset) (- (cdr c) y-offset) '(100 0 0) BKGND_ADD)))
											   (path->list (path e))))
								   (filter (lambda (x) (is-a? x <can-move>)) (entities m))))))