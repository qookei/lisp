(define (die) (car 0))

;; TODO: eq? should support conses
(define (list-equal? a b)
    (if (nil? a) (nil? b)
	(if (nil? b) 0
	    (if (eq? (car a) (car b))
		(list-equal? (cdr a) (cdr b))
		0))))

(define (list . values) values)

;; Make sure unquoting works
(if (list-equal? `(a ,(+ 1 1) c)
		 '(a 2 c))
    0 (die))

;; Make sure basic unquote-splicing works
(if (list-equal? `(a ,@(list 'b 'c 'd) e)
		 '(a b c d e))
    0 (die))

;; Make sure unquote-splicing at the wrong point is left as-is
;; TODO: pending proper eq? support for conses
;;(if (list-equal? `,@(list 'a 'b)
;;		 '(unquote-splicing (list (quote a) (quote b))))
;;    0 (die))

;; Make sure unquote-splicing with an improper list works
;; TODO: pending proper eq? support for conses
;;(if (list-equal? `(1 ,@(cons 2 3))
;;		 '(1 2 . 3))
;;    0 (die))

;; Make sure unquote-splicing with not-a-list works
(if (eq? `(,@1)
	 1)
    0 (die))

;; TODO: currently fails like so:
;; illegal argument: unquote-splicing of a non-cons replaces the outer list, but it's not empty: (1 ())
;; GNU Guile accepts it, so perhaps we ought to as well?
;;(if (* (eq? (car `(1 ,@2)) 1)
;;       (eq? (cdr `(1 ,@2)) 2))
;;    0 (die))
