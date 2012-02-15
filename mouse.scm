(define left-click-hook (make-hook 1))
(define right-click-hook (make-hook 1))
(define middle-click-hook (make-hook 1))

(define add-mouse-hook! (lambda (button proc)
						  (case button
							('left (add-hook! left-click-hook proc))
							('right (add-hook! right-click-hook proc))
							('middle (add-hook! middle-click-hook proc)))))

(define try-mouse-hook (lambda (mouse-info)
						 (if (assoc-ref mouse-info "left-clicked")
							 (run-hook left-click-hook mouse-info))
						 (if (assoc-ref mouse-info "right-clicked")
							 (run-hook right-click-hook mouse-info))
						 (if (assoc-ref mouse-info "middle-clicked")
							 (run-hook middle-click-hook mouse-info))))


(add-mouse-hook! 'left (lambda (mouse-info)
						 (let* ((console-pos (assoc-ref mouse-info "console-pos"))
								(camera (find (lambda (c) (contains-pos? c console-pos)) cameras)))
						   (if camera
							   (set! (position camera) (console-coords->map-coords cam console-pos))
							   (begin
								 (display (cons console-pos camera))
								 (newline)
								 (describe (first cameras))
								 (newline))))))

(add-mouse-hook! 'right (lambda (mouse-info)
						 (let* ((console-pos (assoc-ref mouse-info "console-pos"))
								(camera (find (lambda (c) (contains-pos? c console-pos)) cameras)))
						   (if camera
							   (let ((entities (quadtree-search (quadtree m) (console-coords->map-coords cam console-pos))))
								 (if (null? entities)
									 (begin
									   (clear-camera-following! camera)
									   (add-overlay! camera 'entities (draw-entities-overlay camera)))
									 (set-camera-following! camera (first entities)))
								 (set! player (following camera)))
							   (begin
								 (display (cons console-pos camera))
								 (newline)
								 (describe (first cameras))
								 (newline))))))
