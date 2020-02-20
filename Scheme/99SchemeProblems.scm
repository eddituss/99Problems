;; Solutions to 99 lisp problems in Scheme
;; All solutions assume that the domain is correct, there are no entry-validations
;; Check specification on github.com/eddituss/99Problems

;; Domain:   An element and a list
;; Codomain: A boolean, #t if the element is member of the list, #f otherwise
;; General purpose functions
(define member?
  (lambda (elem l)
    (ormap (lambda (x) (equal? x elem)) l)))


;; Domain: A non-empty list
;; Codomain: The list with only the last element of the input
;; p01
(define last
  (lambda (l)
    (cond ((null? (cdr l)) l)
          (else (last (cdr l))))))

;; Domain: A list with at least 2 elements
;; Codomain: A list with the last 2 elements
;; p02
(define but-last
  (lambda (l)
    (cond ((null? (cddr l)) l)
          (else (but-last (cdr l))))))

;; Domain: A list and a position less than the length of the list
;; Codomain: The element at the n-th position (starting at zero)
;; p03
(define element-at
  (lambda (l n)
    (cond
      ((null? l) l)
      ((zero? n) (car l))
      (else (element-at (cdr l) (- n 1))))))

;; Domain: A list
;; Codomain: A number with the length of the input
;; p04
(define size
  (lambda (l)
    (cond ((null? l) 0)
          (else (+ 1 (size (cdr l)))))))

;; Domain: A list
;; Codomain: The input reversed
;; p05  -  innefficient
(define invert
  (lambda (l)
    (cond ((null? l) l)
          (else (append (invert (cdr l)) (list (car l)))))))

;; Domain:  A list
;; Codomain: A boolean, #t if it is palindrome, #f if it is not
;; p06
(define palindrome?
  (lambda (l)
    (equal? l (invert l))))

;; Domain: A list (may be nested)
;; Codomain: A list with all the input's internal parenthesis "erased"
;; p07
(define flatten
  (lambda (l)
    (cond ((null? l) l)
          ((list? (car l)) (append (flatten (car l)) (flatten (cdr l))))
          (else (cons (car l) (flatten (cdr l)))))))

;; Domain: A list
;; Codomain: The input with every consecutive equal elements placed once
;; p08
(define eliminate-duplicates
  (lambda (l)
    (cond ((null? l) '())
          ((null? (cdr l)) l)
          ((equal? (car l) (cadr l)) (eliminate-duplicates (cdr l)))
          (else (cons (car l) (eliminate-duplicates (cdr l)))))))

;; Domain: A list
;; Codomain: A list of lists each one with the consecutive equal elements in it
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

;; Domain: A list
;; Codomain: A list with the cardinality of the consecutive equal elements
;; p10
(define length-encoding
  (lambda (l)
    (map (lambda (l) (list (length l) (car l))) (pack l))))

;; Domain: A list
;; Codomain: A list with the cardinality of the consecutive equal elements
;; p11
(define length-encoding-1
  (lambda (l)
    (map (lambda (l) (cond ((= 1 (length l)) (car l))
                           (else (list (length l) (car l)))))
         (pack l))))

 
;; p12
;; Domain:  A natural number and an element
;; Codomain: A list of length cant, filled with cant elements
(define repeat
  (lambda (cant elem)
    (cond ((zero? cant) '())
          (else (cons elem (repeat (- cant 1) elem))))))

;; Domain:   A list with lists of the form (cant elem) or just elem (equivalent to (1 elem))
;; Codomain: A list with all the elements unpacked
(define decode
  (lambda (encode)
    (apply append (map (lambda (x) (cond ((list? x) (repeat (car x) (cadr x)))
                                         (else (list x))))
                       encode))))

;; Domain: A list
;; Codomain: A list with the cardinality of the consecutive equal elements 
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

;; Domain:  A list
;; Codomain: The same list with the elements duplied
;; p14
(define dupli
  (lambda (l)
    (cond ((null? l) l)
          (else (append (repeat 2 (car l))
                        (dupli (cdr l)))))))

;; Domain: A list and a natural number cant
;; Codomain: A list with each element replied cant times
;; p15
(define repli
  (lambda (l cant)
    (cond ((null? l) l)
          (else (append (repeat cant (car l))
                        (repli (cdr l) cant))))))

;; Domain:  A list and a natural number n
;; Codomain: A list with every n-th element dropped off
;; p16
(define drop-n
  (lambda (l n)
    (drop-n-aux l n 1)))

(define drop-n-aux
  (lambda (l n cant)
    (cond ((null? l) l)
          ((zero? (remainder cant n)) (drop-n-aux (cdr l) n 1))
          (else (cons (car l) (drop-n-aux (cdr l) n (+ cant 1)))))))

;; Domain:  A list and a natural number less or equal than the length of the list
;; Codomain: A list with two lists, the first with the first part elements, the second with the others
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
;; Domain: A list and a natural number less or equal than the length of the list
;; Codomain: A list with the first k elements of the list
(define first-n
  (lambda (l k)
    (cond ((null? l) '())
          ((zero? k) '())
          (else (cons (car l) (first-n (cdr l) (- k 1)))))))

;; Domain: A list and 2 natural numbers b and e, 0<=b<=e<=length of list
;; Codomain: A sublist from position b to position e
(define slice
  (lambda (l begin end)
    (cond ((null? l) l)
          ((zero? begin) (first-n l (+ 1 end)))
          (else (slice (cdr l) (- begin 1) (- end 1))))))


;; p19
;; Domain:  Two integer numbers
;; Codomain: The natural number r = a(b) that 0<=r<b
(define mod  
  (lambda (a b)
    (remainder (+ b (remainder a b)) b)))

;; Domain: A list and an integer number rot
;; Codomain: A rot rotation of the list (negatives to left)
(define rotate
  (lambda (l rot)
    (rotate-aux l (mod rot (length l)))))


(define rotate-aux
  (lambda (l mod-rot)
    (reorder-and-append (split l mod-rot))))

;; Domain: A list with a pair of lists in the form (l1 l2)
;; Codomain: The list returned appendded in the way (l2 l1)
(define reorder-and-append
  (lambda (splitted)
    (append (cadr splitted) (car splitted))))
          
                                                    
;; Domain:  A list and a natural number less than the length of the list
;; Codomain: The list without the k-th element
;; p20
(define remove-at
  (lambda (l k)
    (cond ((null? l) l)
          ((zero? k) (cdr l))
          (else (cons (car l) (remove-at (cdr l) (- k 1)))))))

;; Domain: A list and a natural number less than the length of the list and an element
;; Codomain: The list with the k-th element inserted
;; p21
(define insert-at
  (lambda (l elem k)
    (cond ((null? l) (list elem))
          ((zero? k) (cons elem l))
          (else (cons (car l) (insert-at (cdr l) elem (- k 1)))))))

;; Domain: Two integer numbers a and b, with a <= b
;; Codomain: An ordered list with all the integers between a and b, inclusive
;; p22
(define range
  (lambda (begin end)
    (cond ((> begin end) '())
          (else (cons begin (range (+ begin 1) end))))))

;; Domain: A list and a natural number less or equal the length of the list
;; Codomain: Cant different elementos randomly selected from the list
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


;; Domain: A two natural numbers, c and t, where c < t
;; Codomain: A list with c randomly selected numbers from 1 to t.
;; p24
(define lotto-select
  (lambda (cant tot)
    (rnd-select cant (range 1 tot))))

;; Domain:  A list
;; Codomain: A random permutation of the list
;; p25
(define rnd-permu
  (lambda (l)
    (rnd-select l (length l))))

;; Domain: A number k and a list, the number must be less or equal the length of the list
;; Codomain: The list of all possible combinations of k elements of the list
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
;; Domain: Two sets A and B
;; Codomain: The set resulting from A-B
(define difference
  (lambda (a b) 
    (cond ((null? a) a)
          ((member? (car a) b) (difference (cdr a) b))
          (else (cons (car a) (difference (cdr a) b))))))

;; Domain: An Element and a lists of lists
;; Codomain: A list with the element inserted in all the sublists (of each list) 
(define place-in
  (lambda (elem ll)
    (cond ((null? ll) ll)
          (else (append (map (lambda (x) (cons elem x)) (car ll))
                        (place-in elem (cdr ll)))))))

;; Domain: A list of elements and a list of natural numbers,
;;         the sum of this numbers must be equal to the length of the first list
;; Codomain: All the possible combinatios of multinomials coefficient
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

;; p28
;; Domain: A list of lists (length-i list-i), a number p, a sublist of length less than p, a sublist of the rest
;; Codomain: Two lists into a list, one with minors, the other with the rest
(define sort-split
  (lambda (len-list piv minor greater)
    (cond ((null? len-list)(list minor greater))
          ((< (caar len-list) piv) (sort-split (cdr len-list) piv (cons (car len-list) minor) greater))
          (else (sort-split (cdr len-list) piv minor (cons (car len-list)  greater))))))

;; Domain:  A list of lists in the form (length-i list-i)
;; Codomain: A sorted lists of length-i based
;; Using quick-sort
(define lsort-aux
  (lambda (len-list)
    (cond ((null? len-list) '())
          (else
           (let ((parti (sort-split (cdr len-list) (caar len-list) '() '())))
             (append
              (lsort-aux (car parti))
              (list (car len-list))
              (lsort-aux (cadr parti))))))))

;; Domain: A lists of lists
;; Codomain: The lists of lists sorted by the lists' length
(define lsort
  (lambda (ll)
    (map cadr (lsort-aux (map (lambda (l) (list (length l) l)) ll)))))










