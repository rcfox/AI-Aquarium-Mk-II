(define virtual-keys (make-hash-table))
(populate-virtual-keys! virtual-keys)

(define key-hooks (make-hash-table))
(define add-key-hook! (lambda (key proc)
						(let ((hook-name (string-append "pressed-" key)))
						  (hash-set! key-hooks hook-name (make-hook 0))
						  (add-hook! (hash-ref key-hooks hook-name) proc))))

(define try-key-hook (lambda (key-info)
					   (let ((key (assoc-ref key-info "c")))
						 (if (and key (not (eq? key #\nul)))
							 (begin
							   (let* ((hook-name (string-append "pressed-" (string key)))
									  (h (hash-ref key-hooks hook-name)))
								 (if (hook? h)
									 (run-hook h))))
							 (begin							   
							   (let* ((hook-name (string-append "pressed-" (hash-ref virtual-keys (assoc-ref key-info "vk"))))
									  (h (hash-ref key-hooks hook-name)))
								 (if (hook? h)
									 (run-hook h))))
							 ))))

(add-key-hook! "q" (lambda ()
					 (set! running (not running))))

(add-key-hook! "f" (lambda ()
					 (for-each (lambda (camera)
								 (if (assoc-ref (overlays camera) 'fov)
									 (remove-overlay! camera 'fov)
									 (add-overlay! camera 'fov (fov-overlay camera))))
							   cameras)))

(add-key-hook! "p" (lambda ()
					 (for-each (lambda (camera)
								 (if (assoc-ref (overlays camera) 'path)
									 (remove-overlay! camera 'path)
									 (add-overlay! camera 'path (path-overlay camera))))
							   cameras)))

;; (add-key-hook! "w" (lambda ()
;; 					 (let ((e (car entities)))
;; 					   (push-goal! e (move-goal e (cons (entity-x e) (1- (entity-y e))))))))

;; (add-key-hook! "s" (lambda ()
;; 					 (let ((e (car entities)))
;; 					   (push-goal! e (move-goal e (cons (entity-x e) (1+ (entity-y e))))))))

;; (add-key-hook! "a" (lambda ()
;; 					 (let ((e (car entities)))
;; 					   (push-goal! e (move-goal e (cons (1- (entity-x e)) (entity-y e)))))))

;; (add-key-hook! "d" (lambda ()
;; 					 (let ((e (car entities)))
;; 					   (push-goal! e (move-goal e (cons (1+ (entity-x e)) (entity-y e)))))))

;; (add-key-hook! "r" (lambda ()
;; 					 (let ((e (car entities)))
;; 					   (push-goal! e (move-goal e (map-random-free-spot m))))))
