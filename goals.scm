(use-modules (oop goops))

(define-class <goal> ()
  (name #:init-value "Goal" #:accessor name #:init-keyword #:name)
  (owner #:accessor owner)
  (prerequisites #:init-value '() #:getter prerequisites #:init-keyword #:prereq))

(define-method (push-goal! (e <has-goals>) (g <goal>))
  (set! (goals e) (cons g (goals e)))
  (set! (owner g) e)
  (for-each (lambda (prereq)
			  (push-goal! e prereq))
			(reverse (prerequisites g))))

(define-method (pop-goal! (e <has-goals>))
  (set! (goals e) (cdr (goals e))))

(define-method (do-goal (e <has-goals>))
  (if (not (null? (goals e)))
	  (if (do-goal (car (goals e)))
		  (pop-goal! e))))

(define-method (do-goal (g <goal>))
  #f)


(define-class <move-goal> (<goal>)
  (coordinates #:init-value '(0 . 0) #:accessor coordinates #:init-keyword #:coords))

(define-method (do-goal (g <move-goal>))
  (let ((e (owner g)))
	(if (not (equal? (coordinates g) (position e)))
		(begin
		  ;; Reset the destination if this goal got interrupted
		  (if (not (equal? (coordinates g) (destination e)))
			  (set! (destination e) (coordinates g)))
		  (set! (position e) (walk-path e))
		  #f)
		#t)))
