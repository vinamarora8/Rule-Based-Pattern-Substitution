;;;; Pattern matcher tells if an expression conforms to a pattern, and if it does then corresponding dictionary is returned

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Recognition of Pattern Variables
;;;
;;; Arbitrary-Expr?
;;; Check if the given symbol (part of a pattern) is an arbitrary expression pattern variable
(define (arbitrary-expr? symb)
	(if	(list? symb)
		(eq? '? (car symb))		; For that it needs to have '? as its first element
		#f))


;;; Arbitrary-Const?
;;; Check if the given symbol (part of a pattern) if an arbitrary constant pattern variable
(define (arbitrary-const? symb)
	(if	(list? symb)
		(eq? '?c (car symb))
		#f))

;;; Arbitrary-Var?
;;; Check if the given symbol (part of a patter) is an arbitrary-variable pattern variable

(define (arbitrary-var? symb)
	(if	(list? symb)
		(eq? '?v (car symb))
		#f))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Recognition of parts of expression
;;;
;;; Lead-Node?
;;; Check if given symbol is a leaf node of the expression
(define (leaf-node? symb)
	(not	(list? symb)))


;;; Constant?
;;; Check if given part of expression is a constant
(define (constant? symb)
	(number? symb))


;;; Variable?
;;; Check if given part of expression is a variable
(define (variable? symb)
	(and	(leaf-node? symb)
		(not (constant? symb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Pattern matching algorithm
;;;
;;; Match
;;; Returns a dictionary after matching

(define (match expr pat dict)
	
		;; First, set an end point for the recursion
	(cond	((null? expr)
			(if	(null? pat)
				dict
				'failed))
		
		;; If extend-dict failed previously, expr and pat don't match
		((eq? dict 'failed)
			'failed)
	
		;; If pat is an arbitrary dict extender, then just extend dictionary and continue
		((arbitrary-expr? (car pat))
			(match 	(cdr expr)
				(cdr pat)
				(extend-dict dict (cadar pat) (car expr))))
		
		((arbitrary-const? (car pat))
			(if	(constant? (car expr))
				(match	(cdr expr)
					(cdr pat)
					(extend-dict dict (cadar pat) (car expr)))
				'failed))
		
		((arbitrary-var? (car pat))
			(if	(variable? (car expr))
				(match	(cdr expr)
					(cdr pat)
					(extend-dict dict (cadar pat) (car expr)))
				'failed))
				

		;; Only start comparing if both expr and pat are simple expressions
		((leaf-node? (car expr))
			(if	(eq? (car pat) (car expr))
					(match	(cdr expr)
						(cdr pat)
						dict)
				'failed))
		
		((leaf-node? (car pat))
			'failed)

		;; If expr is not a simple expression
		(else	(match 	(cdr expr)
				(cdr pat)
				(match 	(car expr)
					(car pat)
					dict)))))
				
