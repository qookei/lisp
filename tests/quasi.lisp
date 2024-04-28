(define (die) (car 0))

(define (list . values) values)

;; Make sure unquoting works
(if (eq? `(a ,(+ 1 1) c)
	 '(a 2 c))
    0 (die))

;; Make sure basic unquote-splicing works
(if (eq? `(a ,@(list 'b 'c 'd) e)
	 '(a b c d e))
    0 (die))

;; Make sure unquote-splicing at the wrong point is left as-is
(if (eq? `,@(list 'a 'b)
	 '(unquote-splicing (list (quote a) (quote b))))
    0 (die))

;; Make sure unquote-splicing with an improper list works
(if (eq? `(1 ,@(cons 2 3))
	 '(1 2 . 3))
    0 (die))

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
