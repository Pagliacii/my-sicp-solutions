(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))
(define (perms seq1 seq2)
  (if (null? seq2)
      '()
      (append
       (map
        (lambda (s)
          (cons s
                (if (pair? (car seq2))
                    (car seq2)
                    (list (car seq2)))))
        seq1)
       (perms seq1 (cdr seq2)))))
(define (nearby? a b)
  (= (abs (- a b)) 1))
(define (ordinary-multiple-dwelling)
  (let* ((c-m       (filter
                     (lambda (t) (> (cadr t) (car t)))
                     (perms '(2 3 4 5) '(3 4 5))))
         (f-c-m     (filter
                     (lambda (t) (and (distinct? t) (not (nearby? (car t) (cadr t)))))
                     (perms '(2 3 4) c-m)))
         (s-f-c-m   (filter
                     (lambda (q) (and (distinct? q) (not (nearby? (car q) (cadr q)))))
                     (perms '(1 2 3 4 5) f-c-m)))
         (b-s-f-c-m (filter
                     distinct?
                     (perms '(1 2 3 4) s-f-c-m))))
    (if (null? b-s-f-c-m)
        false
        (map list '(baker smith fletcher cooper miller) (car b-s-f-c-m)))))
