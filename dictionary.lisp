;;; A lisp script that implements a dictionary meant for pattern matching

;;; Empty dictionary is just an empty list
(define empty-dictionary '())

;;; Dictionary-Failure? : Check if a pair will fail the dictionary
;;; A dictionary is failed if we try to assign a different to an already defined symbol
(define (dict-failure? dict symb val)
	(cond	((null? dict)	0)					; If we have reached the end of dictionary, then dictionary is okay
		(else	(if	(eq? symb (caar dict))			; If symbols match, check for value match
				(if	(eq? val (cadar dict))		; Check if value matches too
					2				; Expression and value both match
					1)				; Expression matches but value doesn't
				(dict-failure? (cdr dict) symb val)))))	; If symbol doesn't match, check further down the dictionary

;;; Extend dictionary:
(define (extend-dict dict symb val)
	(cond	((= (dict-failure? dict symb val) 0) 	; If dictionary doesn't contain the pair
			(cons (cons symb (list val)) dict))
		((= (dict-failure? dict symb val) 1)	; Dictionary contains symbol with different value	
			'failed)
		((= (dict-failure? dict symb val) 2)	; Dictonary contains symbol and value already
			dict)))


;;; To Search the dictionary
;;; Returns the value corresponding to the given symbol
(define (search-dict dict symb)
	(if	(null? dict)
		#f
		(if	(eq? symb (caar dict))
			(cadar dict)
			(search-dict (cdr dict) symb))))
