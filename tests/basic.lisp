(define (die) (car 0))

(if (eq? (car '(1 . 2)) 1)
    0 (die))

(if (eq? (cdr '(1 . 2)) 2)
    0 (die))

(if (eq? (+ 1 2) 3)
    0 (die))

(if (eq? (* 6 4) 24)
    0 (die))

(if (eq? ((lambda () 4)) 4)
    0 (die))

(if (eq? ((lambda (a) (+ a 4)) 2) 6)
    0 (die))
