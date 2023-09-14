#lang eopl
#|
 Estiven Andrés Martínez Granados:2179687
 Jhoimar Silva Torres:2177167
 Ervin Caravali Ibarra:1925648
|#
;;3
;; list-set:
;; Proposito:
;; L x N x X -> L’ : Procedimiento que modifica el valor de un elemento en una lista.
;;
;; Gramática BNF:
;; <lista> := ()
;;          | (<valor-de-scheme> <lista>)
;;
;; Casos de prueba:
;; 1. (list-set '(1 2 3) 0 'a) => (a 2 3)
;; 2. (list-set '(a b c) 2 'x) => (a b x)
;; 3. (list-set '() 0 'a) => ()
;; 4. (list-set '(1 2 3) 3 'a) => (1 2 3)
;; 5. (list-set '(a b c) 1 'x) => (a x c)

(define list-set
  (lambda (L n x)
    ;; Si la lista es vacía, retorna una lista vacía.
    (if (null? L)
        '()
        ;; Si n es 0, reemplaza el primer elemento y conserva el resto.
        (if (= n 0)
            (cons x (cdr L))
            ;; Si n no es 0, conserva el primer elemento y sigue buscando.
            (cons (car L) (list-set (cdr L) (- n 1) x))))))


;;6
;; my-map :
;; Proposito:
;; (X -> Y) x [X] -> [Y] : Aplica una función a cada elemento de una lista.
;;
;; Gramática BNF:
;; <lista> := ()
;;          | (<valor-de-scheme> <lista>)
;;
;; Casos de prueba:
;; 1. (my-map (lambda (x) (+ x 1)) '(1 2 3)) => (2 3 4)
;; 2. (my-map (lambda (x) (* x x)) '(1 2 3)) => (1 4 9)
;; 3. (my-map (lambda (x) (string-append "a" x)) '("b" "c" "d")) => ("ab" "ac" "ad")
;; 4. (my-map (lambda (x) (cons x '())) '(1 2 3)) => ((1) (2) (3))
;; 5. (my-map (lambda (x) (list x x)) '(1 2 3)) => ((1 1) (2 2) (3 3))

(define (my-map func lst)
  ;; Si la lista es vacía, retorna una lista vacía.
  (if (null? lst)
      '()
      ;; Aplica la función a la cabeza y luego aplica recursivamente a la cola.
      (cons ((lambda (x) (func x)) (car lst))
            (my-map func (cdr lst)))))

;;6
;; swapper :
;; Proposito:
;; S1 x S2 x [S] -> [S] : Intercambia dos elementos en una lista.
;;
;; Gramática BNF:
;; <lista> := ()
;;          | (<valor-de-scheme> <lista>)
;;
;; Casos de prueba:
;; 1. (swapper 'a 'b '(a b c)) => (b a c)
;; 2. (swapper 'x 'y '(a b c)) => (a b c)
;; 3. (swapper '1 '2 '(1 2 3)) => (2 1 3)
;; 4. (swapper 'a 'c '(a b a c a)) => (c b a a a)
;; 5. (swapper 'x 'y '()) => ()

(define (swapper E1 E2 L)
  (define (swap elem)
    ;; Si el elemento es igual a E1, lo reemplaza con E2 y viceversa.
    ((lambda (x) (cond ((equal? x E1) E2)
                       ((equal? x E2) E1)
                       (else x))) elem))
  ;; Aplica la función swap a cada elemento de la lista.
  (my-map swap L))

;;9
;; count-inversions :
;; Proposito:
;; [Int] -> Int : Cuenta el número de inversiones en una lista.
;;
;; Gramática BNF:
;; <lista> := ()
;;          | (<valor-de-scheme> <lista>)
;;
;; Casos de prueba:
;; 1. (count-inversions '(1 2 3 4 5)) => 0
;; 2. (count-inversions '(5 4 3 2 1)) => 10
;; 3. (count-inversions '(2 3 8 6 1)) => 5
;; 4. (count-inversions '(1 1 1 1 1)) => 10
;; 5. (count-inversions '(3 1 5 2 4)) => 4

(define count-inversions
  (lambda (lst)
    (cond
      ((null? lst) 0)
      (else (+ (count-lower (car lst) (cdr lst))
               (count-inversions (cdr lst)))))))
;;9
;; count-lower :
;; Proposito:
;; Int x [Int] -> Int : Cuenta cuántos elementos en la lista son menores que x.
;;
;; Gramática BNF:
;; <valor-de-scheme> := 0
;;                   | (<valor-de-scheme>)
;;
;; Casos de prueba:
;; 1. (count-lower 2 '(1 2 3 4 5)) => 2
;; 2. (count-lower 5 '(1 2 3 4 5)) => 5
;; 3. (count-lower 0 '(1 2 3 4 5)) => 0
;; 4. (count-lower 3 '(1 1 1 1 1)) => 5
;; 5. (count-lower 5 '(3 1 5 2 4)) => 5

(define count-lower
  (lambda (x lst)
    (cond
      ((null? lst) 0)
      ((< x (car lst)) (count-lower x (cdr lst)))
      (else (+ 1 (count-lower x (cdr lst)))))))

;;9
;; inversions :
;; Proposito:
;; [Int] -> Int : Cuenta el número de inversiones en una lista.
;;
;; Gramática BNF:
;; <lista> := ()
;;          | (<valor-de-scheme> <lista>)
;;
;; Casos de prueba:
;; 1. (inversions '(1 2 3 4 5)) => 0
;; 2. (inversions '(5 4 3 2 1)) => 10
;; 3. (inversions '(2 3 8 6 1)) => 5
;; 4. (inversions '(1 1 1 1 1)) => 10
;; 5. (inversions '(3 1 5 2 4)) => 4

(define inversions
  (lambda (L)
    (count-inversions L)))

;; 12
;; my-filter :
;; Proposito:
;; (X -> Bool) x [X] -> [X] : Filtra una lista según un predicado.
;;
;; Gramática BNF:
;; <lista> := ()
;;          | (<valor-de-scheme> <lista>)
;;
;; Casos de prueba:
;; 1. (my-filter (lambda (x) (even? x)) '(1 2 3 4 5)) => (2 4)
;; 2. (my-filter (lambda (x) (odd? x)) '(1 2 3 4 5)) => (1 3 5)
;; 3. (my-filter (lambda (x) (> x 2)) '(1 2 3 4 5)) => (3 4 5)
;; 4. (my-filter (lambda (x) (string? x)) '(1 "hello" 3 "world" 5)) => ("hello" "world")
;; 5. (my-filter (lambda (x) (not (list? x))) '(1 (2 3) 3 (4 (5)) 5)) => (1 3 5)

(define (my-filter pred lst)
  (if (null? lst)
      '()
      (if (pred (car lst))
          (cons (car lst) (my-filter pred (cdr lst)))
          (my-filter pred (cdr lst)))))

;;12
;; filter-acum :
;; Proposito:
;; (Int -> Bool) x Int x (Int x Int -> Int) x (Int -> Bool) -> Int
;; Filtra y acumula valores de 'a' a 'b' usando una función de filtro y una función de acumulación.
;;
;; Gramática BNF:
;; <valor-de-scheme> := 0
;;                   | (<valor-de-scheme>)
;;
;; Casos de prueba:
;; 1. (filter-acum 1 5 + 0 even?) => 6
;; 2. (filter-acum 1 10 * 1 odd?) => 945
;; 3. (filter-acum 5 10 + 0 odd?) => 21
;; 4. (filter-acum 1 5 + 0 even?) => 6
;; 5. (filter-acum 1 1 + 0 even?) => 0
(define (filter-acum a b F acum filter)
  (define (recursiva a acum)
    (if (> a b)
        acum
        (if (and (filter a) (my-filter filter (list a)))
            (recursiva (+ a 1) (F acum a))
            (recursiva (+ a 1) acum))))

  (recursiva a acum)
)

;;15
;; count-odd-and-even :
;; Proposito:
;; L -> L : Cuenta la cantidad de números pares e impares en una lista.
;;
;; Gramática BNF:
;; <tree> := ()
;;         | (n <tree> <tree>)
;;
;; Casos de prueba:
;; 1. (count-odd-and-even '()) => (0 0)
;; 2. (count-odd-and-even '(10 () (20 () () (30 () ()))))=>( 2 0)
;; 3. (count-odd-and-even '(10 (5 (2 () ()) (8 () ())) (15 (12 () ()) (20 () ()) (25 () ()))))=>(5 2)
;; 4. (count-odd-and-even '(30 (20 (15 (10 () ()) ()) (25 () ())) (40 () (35 () ()))))=>(4 3)
;; 5. (count-odd-and-even '(50 (40 (30 (20 () ()) ()) (45 () ())) (60 () (55 () ()))))=>(5 2)

(define (count-odd-and-even tree)
  (letrec ((helper 
            (lambda (tree)
              (cond
                ((null? tree) '(0 0))
                (else
                 (let* ((left-count (helper (cadr tree)))
                        (right-count (helper (caddr tree)))
                        (current-value (car tree))
                        (current-odd (if (odd? current-value) 1 0))
                        (current-even (if (even? current-value) 1 0)))
                   (list (+ (car left-count) (car right-count) current-even)
                         (+ (cadr left-count) (cadr right-count) current-odd))))))))
    (helper tree)))

;;18
;; factorial :
;; Proposito:
;; Int -> Int : Calcula el factorial de un número.
;;
;; Gramática BNF:
;; <valor-de-scheme> := 0
;;                   | (<valor-de-scheme>)
;;
;; Casos de prueba:
;; 1. (factorial 0) => 1
;; 2. (factorial 1) => 1
;; 3. (factorial 5) => 120
;; 4. (factorial 10) => 3628800
;; 5. (factorial 3) => 6

(define factorial
  (lambda (n)
    (if (= n 0)
        1
        (* n ((lambda (x) (factorial (- x 1))) n)))))

;;18
;; binomial-coefficient :
;; Proposito:
;; Int x Int -> Int : Calcula el coeficiente binomial.
;;
;; Gramática BNF:
;; <valor-de-scheme> := 0
;;                   | (<valor-de-scheme>)
;;
;; Casos de prueba:
;; 1. (binomial-coefficient 5 2) => 10
;; 2. (binomial-coefficient 10 3) => 120
;; 3. (binomial-coefficient 6 0) => 1
;; 4. (binomial-coefficient 7 7) => 1
;; 5. (binomial-coefficient 4 2) => 6

(define binomial-coefficient
  (lambda (n k)
    (/ (factorial n)
       (* (factorial k)
          (factorial (- n k))))))

;; pascal-element :
;; Proposito:
;; Int x Int -> Int : Calcula un elemento del triángulo de Pascal.
;;
;; Gramática BNF:
;; <valor-de-scheme> := 0
;;                   | (<valor-de-scheme>)
;;
;; Casos de prueba:
;; 1. (pascal-element 4 2) => 6
;; 2. (pascal-element 7 3) => 35
;; 3. (pascal-element 5 0) => 1
;; 4. (pascal-element 6 6) => 1
;; 5. (pascal-element 3 1) => 3

(define pascal-element
  (lambda (row col)
    (binomial-coefficient (- row 1) (- col 1))))

;; pascal-row :
;; Proposito:
;; Int -> [Int] : Genera una fila del triángulo de Pascal.
;;
;; Gramática BNF:
;; <valor-de-scheme> := 0
;;                   | (<valor-de-scheme>)
;;
;; Casos de prueba:
;; 1. (pascal 0) => ()
;; 2. (pascal 1) => (1)
;; 3. (pascal 5) => (1 4 6 4 1)
;; 4. (pascal 7) => (1 6 15 20 15 6 1)
;; 5. (pascal 3) => (1 2 1)

(define pascal
  (lambda (row)
    (define generate-col
      (lambda (col)
        (if (> col row)
            '()
            (cons (pascal-element row col)
                  (generate-col (+ col 1))))))
    (generate-col 1)))












































