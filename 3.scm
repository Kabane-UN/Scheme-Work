; отчивки

; (1) my-flatten без append
(define (my-flatten xs)
  (define (carr xs)
    (if (null? xs)
        xs
        (if (list? (car xs))
            (carr (car xs))
            (car xs))))
  (define (cdrr xs)
    (if (list? (car xs))
        (cons (cdrr (car xs)) (cdr xs))
        (cdr xs)))
  (let loop ((xs xs))
    (if (null? xs)
        xs
        (if (null? (carr xs))
            (loop (cdr xs))
            (cons (carr xs) (loop (cdrr xs)))))))

; (2) list-trim-right без reverse и с линейной соложностью 
(define (list-trim-right xs)
  (let loop ((xs xs) (l '()))
    (if (null? xs)
        xs
        (if (equal? (car xs) '" ")
            (loop (cdr xs) (append l '(" ")))
            (append l (cons (car xs) (loop (cdr xs) l)))))))

; тесты
(my-flatten '((1 2) (5 (3 5)) (4 3)))
(my-flatten '((1 2) 5 (4 3)))
(my-flatten '(() (1 2) () (5 (3 5)) (4 3)))
(list-trim-right '(2 2 2 " " " " 2))
(list-trim-right '(2 2 2 " " " " 2 " " " "))