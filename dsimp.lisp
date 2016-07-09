(load "dictionary.lisp")
(load "matcher.lisp")
(load "instantiator.lisp")
(load "simplifier.lisp")
(load "rules.lisp")

(define dsimp
	(simplifier (append	arithmetic-rules
				deriv-rules
				decrement-rules)))
