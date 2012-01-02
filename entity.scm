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
  (appearance #:init-value (make <map-element> #:r #\? #:f '(255 255 255)) #:accessor appearance #:init-keyword #:appearance))

(define-method (add! (m <map>) (e <entity>))
  (set! (entities m) (cons e (entities m))))

(define-method (rem! (m <map>) (e <entity>))
  (set! (entities m) (delq e (entities m))))

(define-method (distance (e1 <entity>) (e2 <entity>))
  (distance (position e1) (position e2)))

(define-method (distance (p1 <pair>) (p2 <pair>))
  (sqrt (+ (expt (- (car p1) (car p2)) 2) (expt (- (cdr p1) (cdr p2)) 2))))

(define-class <can-move> (<entity>)
  (path #:accessor path)
  (destination #:accessor destination
			   #:allocation #:virtual
			   #:slot-ref (lambda (e)
							(libtcod-path-destination (path e)))
			   #:slot-set! (lambda (e p)
							 (libtcod-path-compute (path e) (position e) p))))

(define-method (add! (m <map>) (e <can-move>))
  (set! (path e) (make-libtcod-path (libtcod-data m)))
  (next-method))

(define-method (walk-path (e <can-move>))
  (libtcod-path-walk (path e) #t))

(define-class <living> (<entity>)
  (health #:init-value 0 #:accessor health #:init-keyword #:health))

(define-class <has-goals> (<entity>)
  (goals #:init-value '() #:accessor goals))

(define-class <has-inventory> (<entity>)
  (inventory #:init-value '() #:accessor inventory))

(define-method (add! (e <has-inventory>) (i <entity>))
  (set! (inventory e) (cons i (inventory e))))

(define-method (rem! (e <has-inventory>) (i <entity>))
  (set! (inventory e) (delq i (inventory e))))

(define-class <person> (<living> <can-move> <has-goals> <has-inventory>)
  (appearance #:init-value (make <map-element> #:r #\@ #:f '(255 0 0)) #:accessor appearance #:init-keyword #:appearance))

(define-class <item> (<entity>)
  (appearance #:init-value (make <map-element> #:r #\$ #:f '(0 255 0)) #:accessor appearance #:init-keyword #:appearance))
