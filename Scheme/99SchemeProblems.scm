;; Solutions to 99 lisp problems in Scheme
;; All solutions assume that the domain is correct, there are no entry-validations
;; Check specification on github.com/eddituss/99Problems

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

;; p14
(define dupli
  (lambda (l)
    (cond ((null? l) l)
          (else (append (repeat 2 (car l))
                        (dupli (cdr l)))))))

;; p15
(define repli
  (lambda (l cant)
    (cond ((null? l) l)
          (else (append (repeat cant (car l))
                        (repli (cdr l) cant))))))

;; p16
(define drop-n
  (lambda (l n)
    (drop-n-aux l n 1)))

(define drop-n-aux
  (lambda (l n cant)
    (cond ((null? l) l)
          ((zero? (remainder cant n)) (drop-n-aux (cdr l) n 1))
          (else (cons (car l) (drop-n-aux (cdr l) n (+ cant 1)))))))

;; p17
(define split
  (lambda (l part)
    (split-aux l part '())))

(define split-aux
  (lambda (l part first)
    (cond ((null? l) (list (reverse first) '()))
          ((zero? part) (list (reverse first) l))
          (else (split-aux (cdr l) (- part 1) (cons (car l) first))))))

;; p18
(define first-n
  (lambda (l k)
    (cond ((null? l) '())
          ((zero? k) '())
          (else (cons (car l) (first-n (cdr l) (- k 1)))))))

(define slice
  (lambda (l begin end)
    (cond ((null? l) l)
          ((zero? begin) (first-n l (+ 1 end)))
          (else (slice (cdr l) (- begin 1) (- end 1))))))

;; p19
(define mod   ;; returns a(b)
  (lambda (a b)
    (remainder (+ b (remainder a b)) b)))

(define rotate
  (lambda (l rot)
    (rotate-aux l (mod rot (length l)))))

(define rotate-aux
  (lambda (l mod-rot)
    (reorder-and-append (split l mod-rot))))

(define reorder-and-append
  (lambda (splitted)
    (append (cadr splitted) (car splitted))))
          
                                                    
;; p20
(define remove-at
  (lambda (l k)
    (cond ((null? l) l)
          ((zero? k) (cdr l))
          (else (cons (car l) (remove-at (cdr l) (- k 1)))))))

;; p21
(define insert-at
  (lambda (l elem k)
    (cond ((null? l) (list elem))
          ((zero? k) (cons elem l))
          (else (cons (car l) (insert-at (cdr l) elem (- k 1)))))))

;; p22
(define range
  (lambda (begin end)
    (cond ((> begin end) '())
          (else (cons begin (range (+ begin 1) end))))))

;; p23

(define rnd-select
  (lambda (l cant)
    (rnd-select-aux l cant '() (random (length l)))))

(define rnd-select-aux
  (lambda (l cant acum pos)
    (cond ((zero? cant) acum)
          ((null? (cdr l)) (cons (car l) acum))
          (else (rnd-select-aux
                 (remove-at l pos)
                 (- cant 1)
                 (cons (element-at l pos) acum)
                 (random (length (cdr l))))))))


;; p24
(define lotto-select
  (lambda (cant tot)
    (rnd-select cant (range 1 tot))))

;; p25
(define rnd-permu
  (lambda (l)
    (rnd-select l (length l))))

;; p26
(define combination
  (lambda (k l)
    (cond ((zero? k) '(()))
          ((= k (length l)) (list l))
          (else
           (append (map (lambda (comb) (cons (car l) comb)) ;; using the first of the list
                        (combination (- k 1) (cdr l)))
                   (combination k (cdr l))))))) ;; not using the first of the list

;; p27
(define difference
  (lambda (a b) ;; return a-b
    (cond ((null? a) a)
          ((member? (car a) b) (difference (cdr a) b))
          (else (cons (car a) (difference (cdr a) b))))))


(define place-in
  (lambda (elem ll)
    (cond ((null? ll) ll)
          (else (append (map (lambda (x) (cons elem x)) (car ll))
                        (place-in elem (cdr ll)))))))

(define group
  (lambda (l distrib)
    (apply append (group-aux l distrib))))

(define group-aux
  (lambda (l groups)
    (cond ((null? groups) '())
          ((null? (cdr groups)) (list (list (combination (car groups) l))))
          (else
           (map (lambda (x)  (place-in  x (group-aux (difference l x) (cdr groups))))
                (combination (car groups) l))))))










;; General purpose functions
(define member?
  (lambda (elem l)
    (ormap (lambda (x) (equal? x elem)) l)))

