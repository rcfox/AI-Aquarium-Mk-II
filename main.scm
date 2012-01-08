(use-modules (oop goops describe)
			 (srfi srfi-1)
			 (ice-9 threads))

(load "util.scm")
(load "map.scm")
(load "entity.scm")
(load "goals.scm")
(load "camera.scm")
(load "overlays.scm")
(load "keys.scm")

(define m (make <cave-map> #:w 80 #:h 60))

(for-each (lambda (x) (add! m (make <person> #:pos (random-free-spot m) #:name (string-append "Guy " (number->string x))
									#:sight (+ 5 (rand-int 5)))))
		  (iota 10))

(for-each (lambda (x) (add! m (make <item> #:pos (random-free-spot m) #:name (string-append "Item " (number->string x)))))
		  (iota 100))

(define player (car (filter (lambda (x) (is-a? x <person>)) (entities m))))
(for-each (lambda (e)
			(push-goal! e (make <wander-goal>))
			(push-goal! e (make <collect-goal> #:type <item>))
			)
		  (filter (lambda (x) (is-a? x <has-goals>)) (entities m)))

(define cam (make <camera> #:w 80 #:h 60 #:map (seen-map player)))
(add-overlay! cam 'fov (fov-overlay-single cam player))
(add-hook! (moved-hook player) (lambda (e) (set! (position cam) (position e))))

(define cameras (list cam))

(define running #t)
(make-thread (lambda ()
			   (init-console 80 60 "Roguelike Test" 10)
			   (while running
				 (for-each (lambda (e) (look e) (do-goal e)) (filter (lambda (x) (is-a? x <has-goals>)) (entities m)))
				 (clear-console)
				 (for-each draw cameras)
				 (flush-console)
				 (try-key-hook (check-keys KEY_PRESSED)))))
