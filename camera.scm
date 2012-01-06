(use-modules (oop goops))

(define-class <camera> ()
  (width #:init-value 0 #:accessor width #:init-keyword #:w)
  (height #:init-value 0 #:accessor height #:init-keyword #:h)

  ;; Screen coordinates, top-left corner
  (screen-x #:init-value 0 #:accessor screen-x #:init-keyword #:screen-x)
  (screen-y #:init-value 0 #:accessor screen-y #:init-keyword #:screen-y)

  (target-map #:accessor target-map #:init-keyword #:map)
  ;; Map coordinates, center of the camera
  (x #:init-value 0 #:accessor x #:init-keyword #:x)
  (y #:init-value 0 #:accessor y #:init-keyword #:y)
  (position #:accessor position
			#:allocation #:virtual
			#:slot-ref (lambda (o)
						 (cons (x o) (y o)))
			#:slot-set! (lambda (o p)
						  (set! (x o) (car p))
						  (set! (y o) (cdr p)))
			#:init-keyword #:pos)

  (overlays #:init-value '() #:accessor overlays))

(define-method (add-overlay! (c <camera>) key overlay)
  (set! (overlays c) (acons key overlay (overlays c))))

(define-method (remove-overlay! (c <camera>) key)
  (set! (overlays c) (assoc-remove! (overlays c) key)))

(define-method (draw-overlays (c <camera>) x-offset y-offset)
  (for-each (lambda (proc)
			  ((cdr proc) x-offset y-offset))
			(overlays c)))

(define-method (draw (c <camera>))
  (let* ((x-offset (- (x c) (/ (width c) 2)))
		 (y-offset (- (y c) (/ (height c) 2)))
		 (screen-x-offset (- x-offset (screen-x c)))
		 (screen-y-offset (- y-offset (screen-y c))))
	(draw (target-map c)
		  (iota (width c) x-offset 1)
		  (iota (height c) y-offset 1)
		  screen-x-offset
		  screen-y-offset)
	(draw-overlays c screen-x-offset screen-y-offset)))

(define-method (on-camera? (c <camera>) i j)
  (and (and (> i (- (x c) (/ (width c) 2)))
			(< i (+ (x c) (/ (width c) 2))))
	   (and (> j (- (y c) (/ (height c) 2)))
			(< j (+ (y c) (/ (height c) 2))))))
