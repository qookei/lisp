(define (die) (car 0))

(define (list . values)
    values)

(define (map fn lst)
    (if (nil? lst)
	'()
	(cons (fn (car lst))
	      (map fn (cdr lst)))))

(define (caar v)
    (car (car v)))

(define (cddr v)
    (cdr (cdr v)))

(define (cadr v)
    (car (cdr v)))

(define (cadar v)
    (car (cdr (car v))))

(define-macro (let bindings body)
    (cons
     ;; Lambda to introduce bindings
     (list 'lambda
	   (map car bindings)
	   body)
     ;; Parameter list for lambda
     (map cadr bindings)))

(let ((a 1)
      (b 2))
  (if (eq? (+ a b) 3)
      0 (die)))


(define (transform-cond-cases cases)
    (if (nil? cases)
	0
	(if (eq? 'else (caar cases))
	    (cadar cases)
	    `(if ,(caar cases)
		 ,(cadar cases)
		 ,(transform-cond-cases (cdr cases))))))

(define-macro (cond . cases)
    (transform-cond-cases cases))

(cond
  ((eq? 1 2) (die))
  ((+ 1 -1) (die))
  (1 0)
  (else (die)))


(define-macro (foo name)
    `(define ,name ',name))

(foo bar)

(if (eq? bar 'bar)
    0 (die))
