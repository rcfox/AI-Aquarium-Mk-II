(use-modules (oop goops describe)
			 (srfi srfi-1)
			 (srfi srfi-98)
			 (ice-9 threads))

(load "util.scm")
(load "map.scm")
(load "entity.scm")
(load "item.scm")
(load "goals.scm")
(load "camera.scm")
(load "overlays.scm")
(load "keys.scm")
(load "mouse.scm")

(define m (make <cave-map> #:w 64 #:h 64))

(for-each (lambda (x) (add! m (make <person> #:pos (random-free-spot m) #:name (string-append "Guy " (number->string x))
									#:sight (+ 5 (rand-int 5))))) (iota 10))

(for-each (lambda (x) (add! m (make <monster> #:pos (random-free-spot m) #:name (string-append "Monster " (number->string x))
									#:sight (+ 5 (rand-int 5)) #:health 10))) (iota 10))

(for-each (lambda (x) (add! m (make <item> #:pos (random-free-spot m) #:name (string-append "Item " (number->string x))))) (iota 100))

(define player (car (filter (lambda (x) (is-a? x <person>)) (entities m))))
(for-each (lambda (e)
			(add-goal! e (make <wander-goal>))
			(add-goal! e (make <explore-goal>))
			(add-goal! e (make <build-building-goal>))
			(for-each (lambda (x) (add-goal! e (make <mine-goal>))) (iota 100))
			(add-goal! e (make <kill-meta-goal> #:type <bad-guy>))
			(add-goal! e (make <collect-goal> #:type <item>))
			)
		  (filter (lambda (x) (is-a? x <person>)) (entities m)))

(for-each (lambda (e)
			(add-goal! e (make <wander-goal>))
			(add-goal! e (make <explore-goal>))
			(add-goal! e (make <kill-meta-goal> #:type <good-guy>))
			(add-goal! e (make <collect-goal> #:type <item>))
			)
		  (filter (lambda (x) (is-a? x <monster>)) (entities m)))

(define cam (make <camera> #:w 64 #:h 64 #:map m))
(add-overlay! cam 'entities (draw-entities-overlay cam))
(let ((x (/ (width m) 2))
	  (y (/ (height m) 2)))
  (set! (position cam) (cons x y)))

(define cameras (list cam))

(define running #t)
(define record-video #f)
(define main-loop (lambda ()
					(init-console 64 64 "Roguelike Test" 10)
					(while #t
					  (try-key-hook (check-keys KEY_PRESSED))
					  (try-mouse-hook (check-mouse))
					  (if running
						  (begin
							(for-each (lambda (e) (look e) (do-goal e)) (filter (lambda (x) (is-a? x <has-goals>)) (entities m))))
						  )
					  (clear-console)
					  (for-each draw cameras)
					  (flush-console)
					  (if record-video
						  (save-screenshot))
					  )))

(if (get-environment-variable "RUNNING_IN_REPL")
	(make-thread main-loop)
	(main-loop))
