;; This should die because we have values to put after a non-cons/nil cdr
`(1 ,@(cons 2 3) 4)
