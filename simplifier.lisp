;;;; Simplify takes a rule set and an expression to produce a simplified expression to first order
(define (simplify rules expr)
	(define foo)
	(cond	;; First, an end point for recursion
		((null? rules)
			expr)
		
		;; Another end point for another type of recursion
		((null? expr)
			expr)
		
		;; If expression is not complex in the first place, 
		;; then return the expression itself
		((leaf-node? expr)
			expr)

		(else
			;; set foo to the dictionary returned by matching
			(set! foo (match expr (caar rules) '() ))

			;; check if car rule is not applicable
			;; i.e if pattern doesn't match the expression
			;; if it doesn't then: Break up the expression into car and cdr
			;; simplify each independently and then cons them back together
			;; But only if car is not a leaf node.
			;;
			;; if it does then replace the expression with a simplified one
			;; and move on to the next rule
			(if 	(eq? foo 'failed)
				(if	(leaf-node? (car expr))
					(simplify	(cdr rules)
							(cons	(car expr)
								(simplify	rules
										(cdr expr))))
					(simplify	(cdr rules)
							(cons	(simplify	rules
										(car expr))
								(simplify	rules
										(cdr expr)))))
				(simplify 	(cdr rules)
						(instantiate	(cadar rules)
								foo
								'()))))))


;;; Simplifier returns a fuction given a rule
;;; That function will repeatedly simplify an expression until no change is observed
(define (simplifier rules)
	
	;; This is the main function here, it will repeatedly simplify until no change
	(define (cont-simplify expr new-expr)
		(if	(equal? expr new-expr)
			expr
			(cont-simplify 	new-expr
					(simplify rules new-expr))))

	(lambda (ex) (cont-simplify '() ex)))
