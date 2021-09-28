; процедуры для множеств

; вспомогательные процедуры
(define (my-filter p xs)
  (if (null? xs)
      xs
      (if (p (car xs))
          (cons (car xs) (my-filter p (cdr xs)))
          (my-filter p (cdr xs)))))
(define (quick-sort xs)
  (if (or (null? xs) (null? (cdr xs)))
      xs
      (append (quick-sort (my-filter (lambda (x) (<= x (car xs))) (cdr xs))) (list (car xs)) (quick-sort (my-filter (lambda (x) (> x (car xs))) (cdr xs))))))

; (1*) list->set реализованный через сортировку
(define (list->setv2 xs)
  (if (null? xs) xs
      (let ((ys (quick-sort xs)))
        (cons (car ys)
              (let loop ((xs (cdr ys)) (a (car ys)))
                (if (null? xs)
                    xs
                    (if (equal? a (car xs))
                        (loop (cdr xs) a)
                        (cons (car xs) (loop (cdr xs) (car xs))))))))))

; (1) list->set универсальный
(define (list->set xs)
  (define (my-filter p xs)
    (if (null? xs)
        xs
        (if (p (car xs))
            (cons (car xs) (my-filter p (cdr xs)))
            (my-filter p (cdr xs)))))
  (let loop ((xs xs))
    (if (null? xs)
        xs
        (cons (car xs) (loop (my-filter (lambda (x) (not (equal? x (car xs)))) (cdr xs)))))))

; (2*) setv2?
(define (setv2? xs) (= (length xs) (length (list->set xs))))

; (2) set?
(define (set? xs) (define (my-element? a xs) (and (not (null? xs)) (or (equal? (car xs) a) (my-element? a (cdr xs))))) (or (null? xs) (and (not (my-element? (car xs) (cdr xs))) (set? (cdr xs)))))

; (3) union
(define (union xs ys) (list->set (append xs ys)))

; (4) intersection
(define (intersection xs ys)
  (define (my-element? a xs) (and (not (null? xs)) (or (equal? (car xs) a) (my-element? a (cdr xs)))))
  (let loop ((xs xs) (ys ys))
    (if (null? xs)
        xs
        (if (my-element? (car xs) ys)
            (cons (car xs) (loop (cdr xs) ys))
            (loop (cdr xs) ys)))))

; (5) difference
(define (difference xs ys)
  (define (my-element? a xs) (and (not (null? xs)) (or (equal? (car xs) a) (my-element? a (cdr xs)))))
  (let loop ((xs xs) (ys ys))
    (if (null? xs)
        xs
        (if (my-element? (car xs) ys)
            (loop (cdr xs) ys)
            (cons (car xs) (loop (cdr xs) ys))))))

; (6) symmetric-difference
(define (symmetric-difference xs ys) (union (difference xs ys) (difference ys xs)))

; (7) set-eq? универсальный
(define (set-eq? xs ys)
  (define (my-element? a xs) (and (not (null? xs)) (or (equal? (car xs) a) (my-element? a (cdr xs)))))
  (or (null? xs) (and (my-element? (car xs) ys) (set-eq? (cdr xs) ys))))

; (7*) set-eq? через сортировку
(define (set-eqv2? xs ys) (equal? (quick-sort xs) (quick-sort ys)))

; тесты
(list->set '(1 1 2 3))
(list->set '(1 1 2 3 33 3 3 3 3 3 3))
(set? '(1 2 3))                              
(set? '(1 2 3 3)) 
(set? '())
(union '(1 2 3) '(2 3 4))
(union '(1 2 3 5) '(2 3 1 4 10))
(intersection '(1 2 3) '(2 3 4))
(intersection '(1 2 3 5 7 8) '(2 3 4 6 8 1))
(difference '(1 2 3 4 5) '(2 3))
(difference '(1 2 3 4 5 6 8 10) '(2 3 8))
(symmetric-difference '(1 2 3 4) '(3 4 5 6))
(symmetric-difference '(1 2 3 4 5 6) '(3 4 5 6 7 10))
(set-eq? '(1 2 3) '(3 2 1))                 
(set-eq? '(1 2) '(1 3))
(set-eq? '(1 2) '())
(list->setv2 '(3 3 3 1 1 2 2 2 2 5))
(set-eqv2? '(1 2 3) '(3 2 1))                 
(set-eqv2? '(1 2) '(1 3))
(set-eqv2? '(1 2) '())