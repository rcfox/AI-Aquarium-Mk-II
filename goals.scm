(use-modules (oop goops))

(define-class <goal> ()
  (name #:init-value "Goal" #:accessor name #:init-keyword #:name)
  (owner #:accessor owner)
  (prerequisites #:init-value '() #:accessor prerequisites #:init-keyword #:prereq))

(define-method (push-goal! (e <has-goals>) (g <goal>))
  (set! (goals e) (cons g (goals e)))
  (set! (owner g) e)
  (for-each (lambda (x) (set! (owner x) e)) (prerequisites g)))

(define-method (pop-goal! (e <has-goals>))
  (let ((head (car (goals e))))
	(set! (goals e) (cdr (goals e)))
	head))

(define-method (do-goal (e <has-goals>))
  (if (not (null? (goals e)))
	  (let ((status (do-goal (car (goals e)))))
		(case status
			  ('cant-do (begin  ;; Do the next goal instead
						  (let ((top-goal (pop-goal! e)))
							(do-goal e)
							(push-goal! e top-goal))))
			  ('progressed '())
			  ('success (pop-goal! e))
			  ('failure (pop-goal! e))))))

(define-method (do-goal (g <goal>))
  (let ((statuses (map do-goal (prerequisites g))))
	(if (null? statuses)
		'success
		(cond
		 ((every (lambda (x) (eq? 'success x)) statuses)
		  'success)
		 ((any (lambda (x) (eq? 'failure x)) statuses)
		  'failure)
		 (#t 'cant-do)))))

(define-class <move-goal> (<goal>)
  (coordinates #:init-value '(0 . 0) #:accessor coordinates #:init-keyword #:coords))

(define-method (do-goal (g <move-goal>))
  (let ((status (next-method)))
	(case status
	  ('success (let ((e (owner g)))
				  (if (equal? (coordinates g) (position e))
					  'success
					  (begin
						;; Reset the destination if this goal got interrupted
						(if (not (equal? (coordinates g) (destination e)))
							(set! (destination e) (coordinates g)))
						(let ((step (walk-path e)))
						  (if step
							  (begin
								(set! (position e) step)
								'progressed)
							  'failure ;; can't proceed
							  ))))))
	  (else status))))

(define-class <get-goal> (<goal>)
  (target #:accessor target #:init-keyword #:target))

(define-method (initialize (g <get-goal>) initargs)
  (next-method)
  (set! (prerequisites g) (cons (make <move-goal> #:coords (position (target g))) (prerequisites g))))

(define-method (do-goal (g <get-goal>))
  (let ((status (next-method)))
	(case status
	  ('success	(let ((e (owner g))
					  (i (target g)))
				  (if (find (lambda (x) (eq? x i)) (entities m)) ;; Make sure the item is still on the map
					  (if (equal? (position i) (position e))
						  (begin
							(add! e i)
							(rem! m i)
							'success)
						  (begin ;; The item moved!
							(push-goal! e (make <move-goal> #:coords (position i)))
							'progressed))
					  'failure)))
	  (else status))))

(define-class <collect-goal> (<goal>)
  (type #:accessor type #:init-keyword #:type)
  (count #:accessor count #:init-value -1 #:init-keyword #:count))

(define-method (do-goal (g <collect-goal>))
  ;; Sort so that we pick the next closest object
  (let ((objects (sort (filter (lambda (x) (is-a? x (type g))) (seen-entities (owner g)))
					   (lambda (a b) (< (distance a (owner g))
										(distance b (owner g)))))))
	(if (null? objects)
		'cant-do
		(begin
		  (if (equal? 0 (count g))
			  'success
			  (begin
				(if (> (count g) 0)
					(set! (count g) (1- (count g))))
				(push-goal! (owner g) (make <get-goal> #:target (car objects)))
				'progressed))))))

(define-class <wander-goal> (<goal>))

(define-method (do-goal (g <wander-goal>))
  (let ((target (find (lambda (x) (> (/ 1 (length (seen-space (owner g)))) (rand-float))) (seen-space (owner g)))))
	(if target
		(push-goal! (owner g) (make <move-goal> #:coords target))
		(push-goal! (owner g) (make <move-goal> #:coords (car (seen-space (owner g)))))))
  'progressed)