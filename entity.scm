(use-modules (oop goops))

(define-class <entity> ()
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
  (name #:init-value "Entity" #:accessor name #:init-keyword #:name)
  (appearance #:init-value (make <map-element> #:r #\@ #:f '(255 0 0)) #:accessor appearance #:init-keyword #:appearance)
  (path #:accessor path)
  (goals #:init-value '() #:accessor goals)
  (destination #:accessor destination
			   #:allocation #:virtual
			   #:slot-ref (lambda (e)
							(libtcod-path-destination (path e)))
			   #:slot-set! (lambda (e p)
							 (libtcod-path-compute (path e) (position e) p))))

(define-method (add! (m <map>) (e <entity>))
  (set! (entities m) (cons e (entities m)))
  (set! (path e) (make-libtcod-path (libtcod-data m))))

(define-method (walk-path (e <entity>))
  (libtcod-path-walk (path e) #t))
