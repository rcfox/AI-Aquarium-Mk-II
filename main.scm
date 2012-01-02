(use-modules (oop goops describe)
			 (srfi srfi-1)
			 (ice-9 threads))

(load "util.scm")
(load "map.scm")
(load "entity.scm")
(load "goals.scm")

(load "overlays.scm")
(load "keys.scm")

(define m (make <map> #:w 80 #:h 60))

(for-each (lambda (x) (add! m (make <person> #:pos (random-free-spot m) #:name (string-append "Guy " (number->string x)))))
		  (iota 10))

(for-each (lambda (x) (add! m (make <item> #:pos (random-free-spot m) #:name (string-append "Item " (number->string x)))))
		  (iota 10))

(define e (car (filter (lambda (x) (is-a? x <person>)) (entities m))))
(for-each (lambda (e)
			(push-goal! e (make <move-goal> #:coords '(10 . 10)
								#:prereq (list (make <move-goal> #:coords '(70 . 50))
											   (make <move-goal> #:coords '(60 . 1))))))
		  (filter (lambda (x) (is-a? x <has-goals>)) (entities m)))

(define running #t)
(make-thread (lambda ()
			   (init-console 80 60 "Roguelike Test" 10)
			   (while running
				 (for-each (lambda (e) (do-goal e)) (filter (lambda (x) (is-a? x <has-goals>)) (entities m)))
				 (clear-console)
				 (draw-map m)
				 (draw-overlays)
				 (flush-console)
				 (try-key-hook (check-keys KEY_PRESSED))
				 )))
