(define (die) (car 0))

(define (list . values)
    values)

(define (map fn lst)
    (if (nil? lst)
	'()
	(cons (fn (car lst))
	      (map fn (cdr lst)))))

(define (cadr v)
    (car (cdr v)))

(define-macro (let bindings body)
    (cons
     ;; Lambda to introduce bindings
     (list 'lambda
	   (map car bindings)
	   body)
     ;; Parameter list for lambda
     (map cadr bindings)))


(define (make-lambda1 arg)
    (let ()
      (lambda () arg)))

(if (eq? ((make-lambda1 10))
	 10)
    0 (die))


(define (make-lambda2 arg)
    (let ((a 1))
      (let ((b 2))
	(lambda () (+ a b arg)))))

(if (eq? ((make-lambda2 15))
	 18)
    0 (die))


(define (make-lambda3 arg1 arg2 arg3)
    (let ((a arg1))
      (let ((b arg2))
	(lambda () (+ a b arg3)))))

(if (eq? ((make-lambda3 4 5 6))
	 15)
    0 (die))
