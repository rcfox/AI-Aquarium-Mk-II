(use-modules (oop goops)
			 (srfi srfi-1)
			 (ice-9 threads))

(load "util.scm")
(load "map.scm")
(load "entity.scm")

(load "overlays.scm")
(load "keys.scm")

(define m (make <cave-map> #:w 80 #:h 60))
(define (draw-map m)
  (for-each-map m (lambda (x y tile)
					(draw-character x y (representation tile) (fore-colour tile) (back-colour tile))))
  (for-each (lambda (e)
  			  (let ((a (appearance e)))
  				(draw-character (x e) (y e) (representation a) (fore-colour a) (back-colour a))))
 			(entities m)))

(for-each (lambda (x) (add! m (make <entity> #:pos (random-free-spot m) #:name (string-append "Entity " (number->string x)))))
		  (iota 10))

(define running #t)
(make-thread (lambda ()
			   (init-console 80 60)
			   (while running
				 (clear-console)
				 (draw-map m)
				 (draw-overlays)
				 (flush-console)
				 (try-key-hook (check-keys KEY_PRESSED))
				 )))
