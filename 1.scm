; процедуры для списков

; (1) my-range
(define (my-range a b d)
  (let loop ((a a))
    (if (>= a b)
        '()
        (cons a (loop (+ a d))))))

; (2) my-flatten
(define (my-flatten xs)
  (let loop ((xs xs))
    (if (null? xs)
        xs
        (append (if (list? (car xs))(my-flatten (car xs)) (list (car xs))) (loop (cdr xs))))))

; (3) my-element?
(define (my-element? a xs) (and (not (null? xs)) (or (equal? (car xs) a) (my-element? a (cdr xs)))))

; (4) my-filter
(define (my-filter p xs)
  (if (null? xs)
      xs
      (if (p (car xs))
          (cons (car xs) (my-filter p (cdr xs)))
          (my-filter p (cdr xs)))))

; (5) my-fold-left
(define (my-fold-left op xs)
  (if (null? (cdr xs))
      (car xs)
      (my-fold-left op (cons (op (car xs) (car (cdr xs))) (cdr (cdr xs))))))

; (6) my-fold-right
(define (my-fold-right op xs)
  (if (null? (cdr xs))
      (car xs)
      (op (car xs) (my-fold-right op (cdr xs)))))

; тесты
(my-range 0 10 2)
(my-range  0 11 3)
(my-flatten '((1 2 3) 2 3))
(my-flatten '((1) 2 (3 (4 5)) 6))
(my-element? 0 '(5 6 10 7 8))
(my-element? 1 '(3 2 1))
(my-element? 4 '(3 2 1))
(my-filter odd? (my-range 0 10 1))
(my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1))
(my-filter null? '(()()()() 1 "fe"))
(my-fold-left + '(1 2 3))
(my-fold-left  quotient '(16 2 2 2 2)) 
(my-fold-left  quotient '(1))
(my-fold-right expt     '(2 3 4))
(my-fold-right expt     '(2))
(my-fold-right quotient     '(2 50 50))