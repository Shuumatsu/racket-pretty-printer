(define a 2)

(multiple-value-bind (a b)
                     (gethash 'a *m*)
                     (list a b))

(check-expect (largest
               (cons -2
                     (cons 2 (cons -5 (cons 1 empty)))))
              2)
