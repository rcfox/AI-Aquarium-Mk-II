(define overlays '())
(define add-overlay (lambda (key overlay)
					  (set! overlays (acons key overlay overlays))))
(define remove-overlay (lambda (key)
						 (set! overlays (assoc-remove! overlays key))))
(define draw-overlays (lambda ()
						(for-each (lambda (proc) ((cdr proc))) overlays)))
