(define deriv-rules
'(
((dd (?c c) (? v))			0)
((dd (?v v) (? v))			1)
((dd (?v u) (? v))			0)

;; Derivative of sum
((dd (+ (? x1) (? x2)) (? v))		(+ 	(dd (: x1) (: v)) 
						(dd (: x2) (: v))))

;; Derivative of product
((dd (*	(? x1) (? x2)) (? v))		(+ 	(* (: x2) (dd (: x1) (: v))) 
						(* (: x1) (dd (: x2) (: v)))))

;; Derivative of power
((dd (^ (? x1) (? n)) (? v))		(*	(: n)
						(*	(^ (: x1) (-1+ (: n)))
							(dd (: x1) (: v)))))
))

(define arithmetic-rules
'(
((+ (? v) 0)				(: v))
((+ 0 (? v))				(: v))
((- (? v) 0)				(: v))
((* (? v) 0)				0)
((* 0 (? v))				0)
((* (? v) 1)				(: v))
((* 1 (? v))				(: v))

((^ (? v) 1)				(: v))
((^ (? v) 0)				(: v))
))

(define decrement-rules
'(
((-1+ 1)				0)
((-1+ 2)				1)
((-1+ 3)				2)
((-1+ 4)				3)
((-1+ 5)				4)
((-1+ 6)				5)))
