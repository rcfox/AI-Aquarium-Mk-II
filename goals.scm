(use-modules (oop goops))

(define-class <goal> ()
  (name #:init-value "Goal" #:accessor name #:init-keyword #:name)
  (owner #:accessor owner #:init-keyword #:owner)
  (priority #:init-value 0 #:accessor priority #:init-keyword #:priority)
  (prerequisites #:init-value '() #:accessor prerequisites #:init-keyword #:prereq)
  (success-hook #:init-form (make-hook 1) #:accessor success-hook)
  (failure-hook #:init-form (make-hook 1) #:accessor failure-hook))

(define-method (add-goal! (e <has-goals>) (g <goal>))
  (set! (owner g) e)
  (set! (goals e) (merge (goals e) (list g) (lambda (a b) (> (priority a) (priority b)))))
  (for-each (lambda (x) (set! (owner x) e)) (prerequisites g)))

(define-method (remove-goal! (e <has-goals>) (g <goal>))
  (set! (goals e) (remove (lambda (x) (eq? g x)) (goals e)))
  g)

(define-method (add-goal! (g <goal>) (subgoal <goal>))
  (set! (owner subgoal) (owner g))
  (set! (prerequisites g) (merge (prerequisites g) (list subgoal) (lambda (a b) (> (priority a) (priority b)))))
  (for-each (lambda (x) (set! (owner x) (owner g))) (prerequisites subgoal)))

(define-method (remove-goal! (g <goal>) (subgoal <goal>))
  (set! (prerequisites g) (remove (lambda (x) (eq? subgoal x)) (prerequisites g)))
  subgoal)

(define-method (do-goal (e <has-goals>))
  (if (not (null? (goals e)))
	  (do-goal e (goals e))))

(define-method (do-goal (e <has-goals>) (goal-list <list>))
  (let* ((g (car goal-list))
		 (status (do-goal g)))
	(case status
	  ('cant-do
	   (do-goal e (cdr goal-list))) ;; try the next goal
	  ('progressed '())
	  ('success
	   (remove-goal! e g)
	   (run-hook (success-hook g) g))
	  ('failure
	   (remove-goal! e g)
	   (run-hook (failure-hook g) g)))))

(define-method (do-goal (g <goal>))
  (let ((statuses (map do-goal (prerequisites g)))) ;; TODO: this will perform all sub-goals, which isn't the intention...
	(if (null? statuses)
		'success ;; there were no prerequisites
		(cond
		 ((every (lambda (x) (eq? 'success x)) statuses)
		  'success)
		 ((any (lambda (x) (eq? 'failure x)) statuses)
		  'failure)
		 ((any (lambda (x) (eq? 'progressed x)) statuses)
		  'progressed)
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

(define-class <follow-goal> (<goal>)
  (target #:accessor target #:init-keyword #:target))

(define-method (do-goal (g <follow-goal>))
  (let ((status (next-method)))
	(case status
	  ('success (let ((e (owner g))
					  (t (target g)))
				  (if (equal? (position t) (position e))
					  'success
					  (begin
						;; Reset the destination if this goal got interrupted
						(if (not (equal? (position t) (destination e)))
							(set! (destination e) (position t)))
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

(define-method (do-goal (g <get-goal>))
  (let ((status (next-method)))
	(case status
	  ('success	(let ((e (owner g))
					  (i (target g)))
				  (if (member i (entities m)) ;; Make sure the item is still on the map
					  (if (equal? (position i) (position e))
						  (begin
							(add! e i)
							(rem! m i)
							'success)
						  (begin
							(add-goal! g (make <move-goal> #:coords (position i)))
							'progressed))
					  'failure)))
	  (else status))))

(define-class <collect-goal> (<goal>)
  (type #:accessor type #:init-keyword #:type)
  (count #:accessor count #:init-value -1 #:init-keyword #:count)
  (priority #:init-value 10 #:accessor priority #:init-keyword #:priority))

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
				(add-goal! (owner g) (make <get-goal> #:target (car objects) #:priority (1+ (priority g))))
				'progressed))))))

(define-class <wander-goal> (<goal>)
  (priority #:init-value -10 #:accessor priority #:init-keyword #:priority))

(define-method (do-goal (g <wander-goal>))
  (let ((target (random-element (filter (lambda (p) (walkable (get-data m p))) (seen-space (owner g))))))
	(if (not target)
		(set! target (car (seen-space (owner g)))))
	(add-goal! (owner g) (make <move-goal> #:coords target #:priority (1+ (priority g)))))
  'progressed)

(define-class <explore-goal> (<goal>)
  (priority #:init-value -5 #:accessor priority #:init-keyword #:priority))

(define-method (do-goal (g <explore-goal>))
  (let ((status (next-method)))
	(case status
	  ('success
	   (let ((target (find
					  (lambda (x) (eq? (representation (get-data (seen-map (owner g)) (car x) (cadr x))) #\ ))
					  (sort (cartesian-product (iota (width m)) (iota (height m)))
							(lambda (a b) (< (distance (list->pair a) (position (owner g)))
											 (distance (list->pair b) (position (owner g)))))))))
		 (set! (prerequisites g) '())
		 (add-goal! g (make <move-goal> #:coords (list->pair target)))
		 'progressed))
	  ('failure
	   'success)
	  (else status))))

(define-class <kill-meta-goal> (<goal>)
  (priority #:init-value 25 #:accessor priority #:init-keyword #:priority)
  (type #:accessor type #:init-keyword #:type))

(define-method (do-goal (g <kill-meta-goal>))
  (let ((objects (sort (filter (lambda (x) (and (not (eq? (owner g) x)) (is-a? x (type g)))) (seen-entities (owner g)))
					   (lambda (a b) (< (distance a (owner g))
										(distance b (owner g)))))))
	(if (null? objects)
		'cant-do
		(begin
		  (add-goal! (owner g) (make <kill-goal> #:target (car objects) #:priority (1+ (priority g))))
		  'progressed))))

(define-class <kill-goal> (<goal>)
  (target #:accessor target #:init-keyword #:target))

(define-method (do-goal (g <kill-goal>))
  (let ((status (next-method)))
	(case status
	  ('success (let ((e (owner g))
					  (i (target g)))
				  (if (member i (entities m)) ;; Make sure the item is still on the map
					  (if (equal? (position i) (position e))
						  (begin
							(if (damage i (rand-int 5))
								'success
								'progressed))
						  (begin ;; The item moved!
							(set! (prerequisites g) '())
							(add-goal! g (make <follow-goal> #:target i))
							'progressed))
					  'failure)))
	  (else status))))

(define-class <mine-goal> (<goal>)
  (priority #:init-value 9 #:accessor priority #:init-keyword #:priority)
  (target #:accessor target #:init-keyword #:target))

(define-method (do-goal (g <mine-goal>))
  (let ((status (next-method)))
	(case status
	  ('success
	   (let ((objects (sort (filter (lambda (x) (eq? (get-data m x) <wall>)) (seen-space (owner g)))
							(lambda (a b) (< (distance a (position (owner g)))
											 (distance b (position (owner g))))))))
		 (if (null? objects)
			 'cant-do
			 (begin
			   (set! (target g) (car objects))
			   (add-goal! g (make <move-goal> #:coords (car objects)))
			   'progressed))))
	  ('failure
	   (if (< (distance (target g) (position (owner g))) 2)
		   (begin
			 (set-data! m (target g) <floor>)
			 'success)
		   'failure))
	  (else status))))