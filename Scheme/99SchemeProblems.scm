
;; Solutions to 99 lisp problems in Scheme
;; All solutions assume that the domain is correct, there are no entry-validations

;; p01
(define last
  (lambda (l)
    (cond ((null? (cdr l)) l)
          (else (last (cdr l))))))

;; p02
(define but-last
  (lambda (l)
    (cond ((null? (cddr l)) l)
          (else (but-last (cdr l))))))

;; p03
(define element-at
  (lambda (l n)
    (cond
      ((null? l) l)
      ((zero? n) (car l))
      (else (element-at (cdr l) (- n 1))))))

;; p04
(define size
  (lambda (l)
    (cond ((null? l) 0)
          (else (+ 1 (size (cdr l)))))))

;; p05  -  innefficient
(define invert
  (lambda (l)
    (cond ((null? l) l)
          (else (append (invert (cdr l)) (list (car l)))))))

;; p06
(define palindrome?
  (lambda (l)
    (equal? l (invert l))))

;; p07
(define flatten
  (lambda (l)
    (cond ((null? l) l)
          ((list? (car l)) (append (flatten (car l)) (flatten (cdr l))))
          (else (cons (car l) (flatten (cdr l)))))))

;; p08
(define eliminate-duplicates
  (lambda (l)
    (cond ((null? l) '())
          ((null? (cdr l)) l)
          ((equal? (car l) (cadr l)) (eliminate-duplicates (cdr l)))
          (else (cons (car l) (eliminate-duplicates (cdr l)))))))

;; p09
(define pack
  (lambda (l)
    (pack-aux (cdr l) (list (car l)))))

(define pack-aux
  (lambda (l act-pack)
    (cond ((null? l) (list act-pack))
          ((equal? (car l) (car act-pack)) (pack-aux (cdr l) (cons (car l) act-pack)))
          (else
           (cons act-pack (pack-aux (cdr l) (list (car l))))))))

;; p10
(define length-encoding
  (lambda (l)
    (map (lambda (l) (list (length l) (car l))) (pack l))))

;; p11
(define length-encoding-1
  (lambda (l)
    (map (lambda (l) (cond ((= 1 (length l)) (car l))
                           (else (list (length l) (car l)))))
         (pack l))))

;; p12
(define repeat
  (lambda (cant elem)
    (cond ((zero? cant) '())
          (else (cons elem (repeat (- cant 1) elem))))))

(define decode
  (lambda (encode)
    (apply append (map (lambda (x) (cond ((list? x) (repeat (car x) (cadr x)))
                                         (else (list x))))
                       encode))))

;; p13
(define encode-direct
  (lambda (l)
    (encode-direct-aux l (car l) 1)))

(define encode-direct-aux
  (lambda (l elem cant)
    (cond ((null? l) (list (list cant elem)))
          ((equal? (car l) elem) (encode-direct-aux (cdr l) elem (+ cant 1)))
          ((= cant 1) (cons elem (encode-direct-aux (cdr l) (car l)  1)))
          (else (cons (list cant elem) (encode-direct-aux (cdr l) (car l)  1))))))
          