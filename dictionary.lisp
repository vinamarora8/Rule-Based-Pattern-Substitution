;;; A lisp script that implements a dictionary meant for patter matching

;;; Empty dictionary is just an empty list
(define empty-dictionary '())

;;; Dictionary-Failure? : Check if a pair will fail the dictionary
;;; A dictionary is failed if we try to assign a different to an already defined expression
(define (dict-failure? dict expr val)
	(cond	((null? dict)	0)					; If we have reached the end of dictionary, then dictionary is okay
		(else	(if	(eq? expr (caar dict))			; If expressions match, check for value match
				(if	(eq? val (cadar dict))		; Check if value matches too
					2				; Expression and value both match
					1)				; Expression matches but value doesn't
				(dict-failure? (cdr dict) expr val)))))	; If expression doesn't match, check further down the dictionary

;;; Extend dictionary:
(define (extend-dict dict expr val)
	(cond	((= (dict-failure? dict expr val) 0) 	; If dictionary doesn't contain the pair
			(cons (cons expr (list val)) dict))
		((= (dict-failure? dict expr val) 1)	; Dictionary contains expression with different value	
			'failed)
		((= (dict-failure? dict expr val) 2)	; Dictonary contains expression and value already
			dict)))

