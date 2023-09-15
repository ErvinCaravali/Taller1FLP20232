#lang eopl
#|
 Estiven Andrés Martínez Granados:2179687
 Jhoimar Silva Torres:2177167
 Ervin Caravali Ibarra:1925648
|#

;;invert
;; 1.
;;Proposito
;; X x Y -> Y' X' Procedimiento que recibe en una lista, y devuelve una lista similar con los pares ordenados invertidos
;;
;;Gramatica BNF:
;;<lista> :=()
;;         |(<valor-de-scheme> <lista>)
;;
;;Pruebas
;;(invert '((a 1) (a 2) (1 b) (2 b))); Deberia imprimir ((1 a) (2 a) (b 1) (b 2))
;;(invert '((5 9) (10 91) (82 7) (a e) ("hola" "Mundo"))); Deberia imprimir ((9 5) (91 10) (7 82) (e a) ("Mundo" "hola"))
;;(invert '(("es" "racket") ("genial" "muy") (17 29) (81 o))); Deberia imprimir (("racket" "es") ("muy" "genial") (29 17) (o 81))
;;(invert '((a z) (b z) (z r))); Deberia imprimir ((z a) (z b) (r z))
;;(invert '(("Hola" "Mundo" "Esto sobra") (1,2,3) (c b a) (b a) (a b)));Deberia imprimir ((a b) (b a))


(define (invert lista)
  ; Función auxiliar para invertir un par (x, y) a (y, x)
  (define invert-pair
    (lambda (pair)
      (list (cadr pair) (car pair))))

  ; Función recursiva para invertir una lista de pares
  (define invert-list
    (lambda (lista acumulador)
      (cond
        [(null? lista) acumulador] ; Si la lista está vacía, devuelve el acumulador
        [else
         (let ((elemento (car lista)))
           (if (= 2 (length elemento))
               (invert-list (cdr lista) (cons (invert-pair elemento) acumulador))
               (begin
                 (display "La lista ")
                 (display elemento)
                 (display " no es de tamaño 2")
                 (newline)
                 (invert-list (cdr lista) acumulador))))])))

  ; Invierte la lista utilizando la función recursiva invert-list
  (reverse (invert-list lista '())))

  ;;2
;;down
;;Proposito:
;;Recibe una lista de objetos y los retorna dentro de paréntesis
;;Casos de prueba:
;; 1. (down '(1 2 3 4)) => '((1) (2) (3) (4))
;; 2. (down '()) => '()
;; 3. (down '(a b c d e f g h i j)) => '((a) (b) (c) (d) (e) (f) (g) (h) (i) (j))
;; 4. (down '(un (objeto (mas)) complicado)) => '((un) ((objeto (mas))) (complicado))
;; 5. (down '(Es que ( yo (soy)) Batman )) => '((Es) (que) ((yo (soy))) (Batman))


(define down
  (lambda (lista)
    (if (null? lista)  ;;Si recibe una lista vacia 
        '()            ;;Retorna solo los paréntesis
        (cons (list (car lista)) ;;Le coloca () al primer elemento 
              (down (cdr lista)))))) ;;

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

;;filter-in
;; 4.
;;Proposito
;; [L] x (P) -> [L'] => P'  Esta funcion filtra los elementos de la lista original segun el criterio proporcionado por el
;;predicado y devuelve una lista con esos elementos que cumplen con ese criterio.
;;
;;Gramatica BNF:
;;<lista> :=()
;;         |(<valor-de-scheme> <lista>)
;;
;;Pruebas
;;(filter-in number? '(a 2 (1 3) b 7));Deberia imprimir (2 7)
;;(filter-in symbol? '(a (b c) 17 foo));Deberia imprimir (a foo)
;;(filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)));Deberia imprimir ("univalle" "racket" "flp")
;;(filter-in symbol? '(a 1 2 3 (9 8 7) b 9 8));Deberia imprimir (a b)
;;(filter-in string? '(a "B" 1 "13" (1 2))); Deberia imprimir ("B" "13")

(define (filter-in P L)
  ; Función auxiliar filter-helper utilizando cálculo lambda y recursión
  (define filter-helper
    (lambda (P L result)
      (cond
        ; Si la lista está vacía, devuelve el resultado invertido
        ((null? L) (reverse result))
        ; Si el primer elemento cumple con el predicado, consérvalo y sigue filtrando el resto de la lista
        ((P (my-car L)) (filter-helper P (my-cdr L) (cons (my-car L) result)))
        ; Si el primer elemento no cumple con el predicado, omítelo y sigue filtrando el resto de la lista
        (else (filter-helper P (my-cdr L) result)))))

  ; Inicializa el resultado como una lista vacía y llama a la función auxiliar
  (filter-helper P L '()))

; Definición de la función personalizada my-car para obtener el primer elemento de una lista
(define my-car
  (lambda (lst)
    (if (null? lst)
        "La lista está vacía"
        (car lst))))

; Definición de la función personalizada my-cdr para obtener el resto de una lista
(define my-cdr
  (lambda (lst)
    (if (null? lst)
        "La lista está vacía"
        (cdr lst))))

;;5
;;list-index
;;Proposito:
#|
Tomar un predicado P de una lista L, si satisface devuleve la posicion del primer elemento
sino devuelve #f
|#
;;Casos de prueba:
#|
1. (list-index number? '(a 2 (1 3) b 7)) => 1
2. (list-index symbol? '(a (b c) 17 foo)) => 0
3. (list-index symbol? '(1 2 (a b) 3)) => #f
4. (list-index number? '(a c (1 3) b 7)) => 4
5. (list-index symbol? '(3 66 (2 4) foo 55)) => 3
|#

(define (list-index P L)
  (define (auxiliar L index check-func)
    (if (null? L)
        #f
        (if (check-func (car L))
            index
            (auxiliar (cdr L) (+ index 1) check-func))))
  
  (auxiliar L 0 (lambda (x) (P x))))

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

;;cartesian-product
;; 7.
;;Proposito
;;[L1]x[L2] -> [L1][L2]  La funcion cartesian-product calcula el producto cartesiano entre dos listas, L1 y L2, y devuelve una
;; lista de pares ordenados que representan todas las combinaciones posibles de elementos de ambas listas.
;;
;;Gramatica BNF:
;;<lista> :=()
;;         |(<valor-de-scheme> <lista>)
;;
;;Pruebas
;;(cartesian-product '(a b c) '(x y));Deberia imprimir ((a x) (a y) (b x) (b y) (c x) (c y))
;;(cartesian-product '(p q r) '(5 6 7)); Deberia imprimir ((p 5) (p 6) (p 7) (q 5) (q 6) (q 7) (r 5) (r 6) (r 7))
;;(cartesian-product '(1 2 3) '(5 4 3 7 8)); Deberia imprimir ((1 5) (1 4) (1 3) (1 7) (1 8) (2 5) (2 4) (2 3) (2 7) (2 8) (3 5) (3 4) (3 3) (3 7) (3 8))
;;(cartesian-product '(0 1) '(2 y)); Deberia imprimir ((0 2) (0 y) (1 2) (1 y))

(define cartesian-product
  (lambda (L1 L2)
    ; Función auxiliar para calcular el producto cartesiano entre dos listas
    (define product
      (lambda (L1 L2)
        (cond
          [(null? L1) '()] ; Si la primera lista está vacía, no hay pares que formar
          [else
           (append-list (cartesian-pairs (car L1) L2) (product (cdr L1) L2))])))

    ; Función auxiliar para crear pares entre un elemento de L1 y todos los elementos de L2
    (define cartesian-pairs
      (lambda (x L2)
        (cond
          [(null? L2) '()] ; Si la segunda lista está vacía, no hay más pares que formar
          [else
           (cons (list x (car L2)) (cartesian-pairs x (cdr L2)))])))

    ; Función auxiliar para concatenar listas
    (define append-list
      (lambda (lst1 lst2)
        (if (null? lst1)
            lst2
            (cons (car lst1) (append-list (cdr lst1) lst2)))))

    (product L1 L2))) ; Llama a la función auxiliar product con las dos listas

;;8
;;mapping
;;Proposito:
;; Crear un mapeo entre 2 listas
;;Casos prueba:
#|
 1. (mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6)) => '((1 2) (2 4) (3 6))
 2. (mapping (lambda (d) (+ d 1)) (list 1 2 3) (list 2 3 4)) => '((1 2) (2 3) (3 4))
 3. (mapping (lambda (d) (+ d 1)) (list 1 2 3) (list 2 3 4)) => '((1 2) (2 3) (3 4))
 4. (mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6)) => '(2 6)
 5.(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12)) => '()
|#

(define (mapping F L1 L2)
  (if (or (null? L1) (null? L2))
      '()
      (if (= ((lambda (x) (F x)) (car L1)) (car L2))
          (cons (list (car L1) (car L2))
                (mapping F (cdr L1) (cdr L2)))
          (mapping F (cdr L1) L2))))

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

;;up
;; 10.
;;Proposito
;; [L]->[L]: La funcion up toma una lista L y devuelve una nueva lista donde se han eliminado los parentesis del nivel mas alto, es decir,
;;quita los parentesis que rodean a los elementos en el nivel superior de la lista. 
;; 
;;Gramatica BNF:
;;<lista> :=()
;;         |(<valor-de-scheme> <lista>)
;;
;;Pruebas
;;(up '((1 2) (3 4)));Deberia imprimir (1 2 3 4)
;;(up '((x (y)) z)); Deberia imprimir (x y z)
;;(up '(z (z y) a)); Deberia imprimir (z z y a)
;;(up '(1 (a b) 3)); Deberia imprimir (up '(1 (a b) 3))

(define up
  (lambda (L)
    ; Función auxiliar para procesar un elemento del nivel más alto de la lista.
    (define procesar-elemento
      (lambda (elemento)
        (cond
          [(list? elemento) ; Si el elemento es una lista
           (procesar-lista elemento)] ; Llamar a procesar-lista.
          [else (list elemento)]))) ; Si el elemento no es una lista, incluirlo tal cual en una lista.

    ; Función auxiliar para procesar una lista y aplicar procesar-elemento a cada elemento.
    (define procesar-lista
      (lambda (lista)
        (cond
          [(null? lista) '()] ; Si la lista está vacía, devuelve una lista vacía.
          [else
           (append (procesar-elemento (car lista)) ; Agregar el resultado de procesar-elemento del primer elemento.
                   (procesar-lista (cdr lista)))]))) ; Llamar recursivamente a procesar-lista con el resto de la lista.

    (procesar-lista L))) ; Procesa la lista completa.

;;11
;;zip
;;Proposito:
;;Combinar 2 listas con las que se pueda hacer una operacion es especifico
#|
Casos prueba:
(zip + '(1 4) '(6 2)) => '(7 6)
(zip * '(11 5 6) '(10 9 8)) => '(110 45 48)
(zip + '(1 2 3) '(4 5 6)) => '(5 7 9)
(zip - '(10 20 30) '(2 4 6)) => '(8 16 24)
(zip * '(1 2 3) '(2 3 4)) => '(2 6 12)
|#

(define (zip F L1 L2)
  (if (or (null? L1) (null? L2))
      '()
      (cons ((lambda (x y) (F x y)) (car L1) (car L2))
            (zip F (cdr L1) (cdr L2)))))

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

;;(operate lrators lrands)
;; 13.
;;Proposito
;; [Irators]x[Irands]->[Irators][Irands]: la funci�n operate calcula el resultado de aplicar sucesivamente operadores binarios a los
;;operandos proporcionados, siguiendo el orden de los operadores en lrators.
;; 
;;Gram�tica BNF:
;;<lista> :=()
;;         |(<valor-de-scheme> <lista>)
;;
;;Pruebas
;;(operate (list + * + - *) '(1 2 8 4 11 6)); Debe imprimir 102
;;(operate (list *) '(4 5)); Debe imprimir 20
;;(operate (list * -) '(4 4 2)); Debe imprimir 14
;;(operate (list + - * *) '(1 1 1 3 9)); Debe imprimir 27

(define operate
  (lambda (lrators lrands)
  
  (define apply-operator
    (lambda (operador operando1 operando2)
    (operador operando1 operando2)))

  (define apply-operators
    (lambda (operadores operandos)
    (if (null? operadores)
        (car operandos)
        (apply-operators (cdr operadores)
                         (cons (apply-operator (car operadores) (car operandos) (cadr operandos))
                               (cddr operandos))))))

  (apply-operators lrators lrands)

  ))

;;14
;;path
;;Proposito;
;;devuelve una lista de "left" y "right" que te guía a través del árbol desde la raíz hasta el nodo que contiene n.
#|
1. (path 17 '(14 (7 () (12 () ()))
(26 (20 (17 () ())
())
(31 () ())))) => '(right left left)
2. (path 14 '(14 () ())) => '()
3. (path 12 '(14 (7 () ()) => '(left)
(12 () ())))
4. (path 20 '(14 (7 () ()) => '(right left)
 (26 (20 (17 () ())
 ()) (31 () ()))))
5. (path 15 '(14 (7 () ()) => '()
(26 (20 (17 () ()) ())
 (31 () ()))))
|#

(define (path n BST)
  (define (auxiliar-path tree path-so-far)
    (cond
      ((null? tree) '())                
      (((lambda (x) (= x n)) (car tree)) path-so-far)   
      (((lambda (x) (< x n)) (car tree))             
       (auxiliar-path (cadr tree) (append path-so-far '(left))))
      (else                             
       (auxiliar-path (caddr tree) (append path-so-far '(right))))))
  
  (if (null? BST)
      '()                               
      (auxiliar-path BST '())))

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

;;(Operar-binarias operacionB)
;; 16.
;;Proposito
;; (operador)x(operando1)x(operando2)->(operacionB):La funci�n operate realiza una serie de c�lculos binarios sucesivos seg�n los operadores y operandos proporcionados.
;; 
;;Gramatica BNF:
;;<OperacionB>::= <int>
;;::= (<OperacionB> �suma <OperacionB>)
;;::= (<OperacionB> �resta <OperacionB>)
;;::= (<OperacionB> �multiplica <OperacionB>)
;;
;;
;;Pruebas
;;Operar-binarias 4); Deberia imprimir 4
;;(Operar-binarias '(2 suma 9) );Deberia imprimir 11
;;(Operar-binarias '(2 resta 9) );Deberia imprimir -7
;;(Operar-binarias '(2 multiplica 9) );Deberia imprimir 18
;;(Operar-binarias '( (2 multiplica 3) suma (5 resta 1 ) ) );Deberia imprimir 10
;;(Operar-binarias '( (2 multiplica (4 suma 1) )
;;multiplica
;;( (2 multiplica 4) resta 1 ) ) );Deberia imprimir 70
;;(Operar-binarias '(1 suma 4));Deberia imprimir 5
;;(Operar-binarias '(1 resta 205));Deberia imprimir -204

(define Operar-binarias
  (lambda (operacionB)
  (cond
    ((number? operacionB) operacionB) ; Caso base: si es un numero, se retorna el numero
    ((list? operacionB)
     (let* ((operador (cadr operacionB))
            (operando1 (Operar-binarias (car operacionB)))
            (operando2 (Operar-binarias (caddr operacionB))))
       (cond
         ((eq? operador 'suma) (+ operando1 operando2))
         ((eq? operador 'resta) (- operando1 operando2))
         ((eq? operador 'multiplica) (* operando1 operando2))
         (else ("Operador no valido")))))
    (else ("Operacion binaria no valida")))))

;;17
;;prod-scalar-matriz mat vec
;;Proposito:
;;
#|
Casos prueba:
1. (prod-scalar-matriz '((1 1) (2 2)) '(2 3)) => '((2 3) (4 6))
2. (prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3)) => '((2 3) (4 6) (6 9))
3. (prod-scalar-matriz '((1 1) (2 2)) '(2 3)) => '((2 3) (4 6))
4. (prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3)) => '((2 3) (4 6) (6 9))
5. (prod-scalar-matriz '((4 5 6) (7 8 9) (10 11 12)) '(1 2 3)) => '((4 10 18) (7 16 27) (10 22 36))
|#

(define (prod-scalar-matriz mat vec)
  (define (producto-escalar row vec)
    (if (null? row)
        '()
        (cons ((lambda (x y) (* x y)) (car row) (car vec))
              (producto-escalar (cdr row) (cdr vec)))))
  
  (if (null? mat)
      '()
      (cons (producto-escalar (car mat) vec)
            (prod-scalar-matriz (cdr mat) vec))))    

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
