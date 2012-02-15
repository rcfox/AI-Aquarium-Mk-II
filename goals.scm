(use-modules (oop goops))

(define-class <goal> ()
  (name #:init-value "Goal" #:accessor name #:init-keyword #:name)
  (owner #:accessor owner #:init-keyword #:owner)
  (priority #:init-value 0 #:accessor priority #:init-keyword #:priority)
  (prerequisites #:init-value '() #:accessor prerequisites #:init-keyword #:prereq)
  (success-hook #:init-form (make-hook 1) #:accessor success-hook)
  (failure-hook #:init-form (make-hook 1) #:accessor failure-hook)
  (status #:init-value 'not-started #:accessor status))

(define-method (add-goal! (e <has-goals>) (g <goal>))
  (set! (owner g) e)
  (init-goal g)
  (set! (goals e) (merge (goals e) (list g) (lambda (a b) (> (priority a) (priority b)))))
  (for-each (lambda (x) (set! (owner x) e)) (prerequisites g)))

(define-method (remove-goal! (e <has-goals>) (g <goal>))
  (set! (goals e) (remove (lambda (x) (eq? g x)) (goals e)))
  g)

(define-method (add-goal! (g <goal>) (subgoal <goal>))
  (set! (owner subgoal) (owner g))
  (init-goal subgoal)
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

(define-method (init-goal (g <goal>))
  #f)

(define-method (reset-goal (g <goal>))
  (set! (status g) 'not-started))

(define-method (do-goal (g <goal>))
  (let ((receiver (lambda (exit-loop)
					(for-each (lambda (p)
								(if (not (or (eq? (status p) 'success) (eq? (status p) 'failure)))
									(begin
									  (set! (status p) (do-goal p))
									  (if (not (eq? (status p) 'cant-do))
										  (exit-loop (status p))))))
							  (prerequisites g)))))
	(call/cc receiver))
  (let ((statuses (map status (prerequisites g))))
	(if (null? statuses)
		'success ;; there were no prerequisites
		(cond
		 ((every (lambda (x) (eq? 'success x)) statuses)
		  'success)
		 ((any (lambda (x) (eq? 'failure x)) statuses)
		  'failure)
		 ((any (lambda (x) (eq? 'progressed x)) statuses)
		  'progressed)
		 ((any (lambda (x) (eq? 'not-started x)) statuses)
		  'cant-do)
		 (#t 'cant-do)))))

(define-class <move-goal> (<goal>)
  (coordinates #:init-value '(0 . 0) #:accessor coordinates #:init-keyword #:coords)
  (max-distance #:init-value 0 #:accessor max-distance #:init-keyword #:max-dist))

(define-method (do-goal (g <move-goal>))
  (let ((status (next-method)))
	(case status
	  ('success (let ((e (owner g)))
				  (if (<= (distance (coordinates g) (position e)) (max-distance g))
					  'success
					  (begin
						;; Reset the destination if this goal got interrupted
						(if (not (equal? (coordinates g) (destination e)))
							(set! (destination e) (coordinates g)))
						(let ((step (walk-path e)))
						  (if step
							  (begin
								(move! e step)
								'progressed)
							  'failure ;; can't proceed
							  ))))))
	  (else status))))

(define-class <follow-goal> (<goal>)
  (target #:accessor target #:init-keyword #:target)
  (max-distance #:init-value 1 #:accessor max-distance #:init-keyword #:max-dist))

(define-method (do-goal (g <follow-goal>))
  (let ((status (next-method)))
	(case status
	  ('success (let ((e (owner g))
					  (t (target g)))
				  (if (<= (distance (position t) (position e)) (max-distance g))
					  'success
					  (begin
						;; Reset the destination if this goal got interrupted
						(if (not (equal? (position t) (destination e)))
							(set! (destination e) (position t)))
						(let ((step (walk-path e)))
						  (if step
							  (begin
								(move! e step)
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
					  (if (< (distance (position i) (position e)) 2)
						  (begin
							(if (damage i (rand-int 5))
								'success
								'progressed))
						  (begin ;; The item moved!
							(set! (prerequisites g) '())
							(add-goal! g (make <follow-goal> #:target i #:max-dist 1.5))
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

(define-class <find-clear-space-goal> (<goal>)
  (width #:accessor width #:init-keyword #:w)
  (height #:accessor height #:init-keyword #:h))

(define-method (do-goal (g <find-clear-space-goal>))
  (define space-clear? (lambda (pos)
						 (every (lambda (elem) (eq? elem <floor>))
								(map->list (seen-map (owner g))
										   (lambda (x y data) data)
										   (iota (width g) (car pos))
										   (iota (height g) (cdr pos))))))
  (if (space-clear? (position (owner g)))
	  'success
	  'cant-do))

(define-class <place-wall-goal> (<goal>)
  (coordinates #:accessor coordinates #:init-keyword #:coords))

(define-method (init-goal (g <place-wall-goal>))
  (add-goal! g (make <move-goal> #:coords (coordinates g) #:max-dist 1.5)))

(define-method (do-goal (g <place-wall-goal>))
  (let ((status (next-method)))
	(case status
	  ('success
	   (if (eq? (get-data m (coordinates g)) <unmineable-wall>)
		   'success
		   (begin
			 (set-data! m (coordinates g) <unmineable-wall>)
			 'success)))
	  ('failure
	   (if (eq? (get-data m (coordinates g)) <unmineable-wall>)
		   'success
		   (begin
			 (for-each reset-goal (prerequisites g))
			 'cant-do)))
	  (else status))))

(define-class <build-building-goal> (<goal>)
  (priority #:init-value 10 #:accessor priority #:init-keyword #:priority)
  (width #:init-value 5 #:accessor width #:init-keyword #:w)
  (height #:init-value 5 #:accessor height #:init-keyword #:h)
  (state #:init-value 'find-space #:accessor state))

(define-method (init-goal (g <build-building-goal>))
  (add-goal! g (make <find-clear-space-goal> #:w (width g) #:h (height g))))

(define-method (do-goal (g <build-building-goal>))
  (let ((status (next-method)))
	(case status
	  ('success
	   (case (state g)
		 ('find-space
		  (set! (prerequisites g) '())
		  (for-each (lambda (p)
					  (add-goal! g (make <place-wall-goal> #:coords p)))
					(append (map->list (seen-map (owner g))
									   (lambda (x y data) (cons x y))
									   (iota (width g) (car (position (owner g))))
									   (iota 1 (cdr (position (owner g)))))
							(reverse (map->list (seen-map (owner g))
												(lambda (x y data) (cons x y))
												(iota 1 (1- (+ (width g) (car (position (owner g))))))
												(iota (height g) (1- (+ (height g) (cdr (position (owner g))))) -1)))
							(map->list (seen-map (owner g))
									   (lambda (x y data) (cons x y))
									   (iota (width g) (1- (+ (width g) (car (position (owner g))))) -1)
									   (iota 1 (1- (+ (height g) (cdr (position (owner g)))))))
							(reverse (map->list (seen-map (owner g))
												(lambda (x y data) (cons x y))
												(iota 1 (car (position (owner g))))
												(iota (height g) (cdr (position (owner g))))))))
		  (set! (state g) 'building)
		  'progressed)
		 ('building
		  'success))
	   )
	  (else status))))
