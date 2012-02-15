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
						 (display (assoc-ref mouse-info "console-pos"))
						 (newline)))
