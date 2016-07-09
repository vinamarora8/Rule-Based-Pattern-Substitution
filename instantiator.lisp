;;;; Instantiator will take a dictionary and a skeleton to produce an expression

;;; Replacer?
;;; Checks if given symbol is a replacer or not
(define (replacer? symb)
	(if 	(list? symb)
		(eq? ': (car symb))
		#f))

;;; Main Algorithm
;;; Instatiate
(define (instantiate skeleton dict expr)
	
	(cond	;; Limit for recursion
		((null? skeleton)
			expr)
		
		((not	(list? skeleton))
			(append expr skeleton))
		;	(instantiate	(list skeleton)
		;			dict
		;			expr))
		
		((replacer? skeleton)
			(append	
				(if	(list? expr)
					expr
					(list expr))
				(search-dict dict (cadr skeleton))))

		;; If currently we are at a replacer
		((replacer? (car skeleton))
			(instantiate	(cdr skeleton)
					dict
					(append 
						(if	(list? expr)
							expr
							(list expr))
						(list (search-dict dict (cadar skeleton))))))
		
		;; If currently we are a leaf node
		((leaf-node? (car skeleton))
			(instantiate	(cdr skeleton)
					dict
					(append 
						(if	(list? expr)
							expr
							(list expr))
						(car skeleton))))


		;; If we have reached so far, it means
		;; we are currently neither at a leaf node nor a replacer
		;; i.e. we are at the start of a complex expression bracket
		(else	(instantiate	(cdr skeleton)
					dict
					(append 
						(if 	(list? expr)
							expr
							(list expr))
						(list (instantiate	(car skeleton)
									dict
									'() )))))))
