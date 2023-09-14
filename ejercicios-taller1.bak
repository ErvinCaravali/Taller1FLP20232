#lang eopl

#|
   Taller 1 FLP
   Ervin Caravali Ibarra 1925648
   Jhoimar Enrique Silva 2177167
   Estiven Andres Martinez G 2179687
|#

;;invert
;; 1.
;;Proposito
;; X x Y -> Y' X' Procedimiento que recibe en una lista, y devuelve una lista similar con los pares ordenados invertidos
;;
;;Gramática BNF:
;;<lista> :=()
;;         |(<valor-de-scheme> <lista>)
;;
;;Pruebas
;;(display (invert '(("es" "racket" "A") ("genial" "muy") (17 29) (81 o))))
;;(display (invert '((5 9) (10 91) (82 7) (a e) ("hola" "Mundo" "MiMundoHola"))))
;;(display (invert '((a 1) (a 2) (1 b) (2 b))))
;;(display (invert '((a c) (d f) (g i) (j L))))
;;(display (invert '((1 2) (1 2) (1 1) (0 1))))

(define (invert lista)
  ; Función auxiliar para invertir un par (x, y) a (y, x)
  (define (invert-pair pair)
    (list (cadr pair) (car pair)))

  ; Función recursiva para invertir una lista de pares
  (define (invert-list lista acumulador)
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
               (invert-list (cdr lista) acumulador))))]))

  ; Invierte la lista utilizando la función recursiva invert-list
  (reverse (invert-list lista '())))

------------------------------------------------------------------------------------------

#lang eopl

#|
   Taller 1 FLP
   Ervin Caravali Ibarra 1925648
   Jhoimar Enrique Silva 2177167
   Estiven Andres Martinez G 2179687
|#

;;filter-in
;; 4.
;;Proposito
;; lst x [L] x (P) -> [L'] => P'  Está función filtra los elementos de la lista original según el criterio proporcionado por el
;;predicado y devuelve una lista con esos elementos que cumplen con ese criterio.
;;
;;Gramática BNF:
;;<lista> :=()
;;         |(<valor-de-scheme> <lista>)
;;
;;Pruebas
;;(display (filter-in number? '(a 2 (1 3) b 7))) ; Filtra números
;(display (filter-in symbol? '(a (b c) 17 foo))) ; Filtra símbolos
;;(display (filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))) ; Filtra cadenas
;;(display (filter-in number? '(a 1 (1 9) c 1)))
;;(display (filter-in symbol? '(a b 123 "hola" 'c d 'mundo)))

; Definición de una función personalizada my-car para obtener el primer elemento de una lista
(define (my-car lst)
  (if (null? lst)
      ( "La lista está vacía")
      (car lst)))

; Definición de una función personalizada my-cdr para obtener el resto de una lista
(define (my-cdr lst)
  (if (null? lst)
      ( "La lista está vacía")
      (cdr lst)))

; Definición de la función filter-in que toma un predicado P y una lista L como argumentos
(define (filter-in P L)
  ; Definición de una función auxiliar llamada filter-helper
  (define (filter-helper P L result)
    (cond
      ; Si la lista está vacía, devuelve el resultado invertido
      [(null? L) (reverse result)]
      ; Si el primer elemento cumple con el predicado, consérvalo y sigue filtrando el resto de la lista
      [(P (my-car L)) (filter-helper P (my-cdr L) (cons (my-car L) result))]
      ; Si el primer elemento no cumple con el predicado, omítelo y sigue filtrando el resto de la lista
      [else (filter-helper P (my-cdr L) result)]))

  ; Inicializa el resultado como una lista vacía y llama a la función auxiliar
  (filter-helper P L '()))

----------------------------------------------------------------------------------------

#lang eopl

#|
   Taller 1 FLP
   Ervin Caravali Ibarra 1925648
   Jhoimar Enrique Silva 2177167
   Estiven Andres Martinez G 2179687
|#

;;cartesian-product
;; 7.
;;Proposito
;; lst1 x lst2 x[L1]x[L2] -> [L1][L2]  La función cartesian-product calcula el producto cartesiano entre dos listas, L1 y L2, y devuelve una
;; lista de pares ordenados que representan todas las combinaciones posibles de elementos de ambas listas.
;;
;;Gramática BNF:
;;<lista> :=()
;;         |(<valor-de-scheme> <lista>)
;;
;;Pruebas
;;(display "(cartesian-product '(a b c) '(x y)): ")
;;(display (cartesian-product '(a b c) '(x y))) ; Debería imprimir ((a x) (a y) (b x) (b y) (c x) (c y))
;;(display "(cartesian-product '(p q r) '(5 6 7)): ")
;;(display (cartesian-product '(p q r) '(5 6 7))) ; Debería imprimir ((p 5) (p 6) (p 7) (q 5) (q 6) (q 7) (r 5) (r 6) (r 7))

(define (cartesian-product L1 L2)
  ; Función auxiliar para calcular el producto cartesiano entre dos listas
  (define (product L1 L2)
    (cond
      [(null? L1) '()] ; Si la primera lista está vacía, no hay pares que formar
      [else
       (append-list (cartesian-pairs (car L1) L2) (product (cdr L1) L2))]))

  ; Función auxiliar para crear pares entre un elemento de L1 y todos los elementos de L2
  (define (cartesian-pairs x L2)
    (cond
      [(null? L2) '()] ; Si la segunda lista está vacía, no hay más pares que formar
      [else
       (cons (list x (car L2)) (cartesian-pairs x (cdr L2)))]))

  ; Función auxiliar para concatenar listas
  (define (append-list lst1 lst2)
    (if (null? lst1)
        lst2
        (cons (car lst1) (append-list (cdr lst1) lst2))))

  (product L1 L2)) ; Llama a la función auxiliar product con las dos listas


-------------------------------------------------------------------------------------------

#lang eopl

#|
   Taller 1 FLP
   Ervin Caravali Ibarra 1925648
   Jhoimar Enrique Silva 2177167
   Estiven Andres Martinez G 2179687
|#

;;up
;; 10.
;;Proposito
;; [L]->[L]: La función up toma una lista L y devuelve una nueva lista donde se han eliminado los paréntesis del nivel más alto, es decir,
;;quita los paréntesis que rodean a los elementos en el nivel superior de la lista. 
;; 
;;Gramática BNF:
;;<lista> :=()
;;         |(<valor-de-scheme> <lista>)
;;
;;Pruebas
;;(display (up '((1 2) (3 4)))) ; Debería imprimir (1 2 3 4)
;;(display (up '((x (y)) z))) ; Debería imprimir (x (y) z)
;;(display (up '((x (12345) (y)) z))) ;Debería imprimir (x 12345 y z)
;;(display (up '(a '(5*8/3))));Debería imprimir (a quote 5*8/3)



; up: Lista -> Lista
; Definición de la función up que recibe una lista L y devuelve una lista con paréntesis removidos del nivel más alto.
(define (up L)
  ; procesar-elemento: Elemento -> Lista
  ; Función auxiliar que procesa un elemento del nivel más alto de la lista.
  (define (procesar-elemento elemento)
    (cond
      [(list? elemento) ; Si el elemento es una lista
       (procesar-lista elemento)] ; Llamar a procesar-lista.
      [else (list elemento)])) ; Si el elemento no es una lista, incluirlo tal cual en una lista.

  ; procesar-lista: Lista -> Lista
  ; Función auxiliar que procesa una lista y aplica procesar-elemento a cada elemento de la lista.
  (define (procesar-lista lista)
    (cond
      [(null? lista) '()] ; Si la lista está vacía, devuelve una lista vacía.
      [else
       (append (procesar-elemento (car lista)) ; Agregar el resultado de procesar-elemento del primer elemento.
               (procesar-lista (cdr lista)))])) ; Llamar recursivamente a procesar-lista con el resto de la lista.

  (procesar-lista L)) ; Procesa la lista completa.

---------------------------------------------------------------------------------------------

#lang eopl

#|
   Taller 1 FLP
   Ervin Caravali Ibarra 1925648
   Jhoimar Enrique Silva 2177167
   Estiven Andres Martinez G 2179687
|#

;;(operate lrators lrands)
;; 13.
;;Proposito
;; [Irators]x[Irands]->[Irators][Irands]: la función operate calcula el resultado de aplicar sucesivamente operadores binarios a los
;;operandos proporcionados, siguiendo el orden de los operadores en lrators.
;; 
;;Gramática BNF:
;;<lista> :=()
;;         |(<valor-de-scheme> <lista>)
;;
;;Pruebas
;;(display (operate (list + * + - *) '(1 2 8 4 11 6))) ; Debe imprimir 102
;;(display (operate (list *) '(4 5))) ; Debe imprimir 20
;;(display (operate (list - / +) '(10 2 5 3))) ; Debe imprimir 0
;;(display (operate (list * + - /) '(3 2 4 2 2))) ; Debe imprimir 6

(define (operate lrators lrands)
  (define (apply-operator operador operando1 operando2)
    (operador operando1 operando2))
  
  (define (apply-operators operadores operandos)
    (if (null? operadores)
        (car operandos)
        (apply-operators (cdr operadores)
                         (cons (apply-operator (car operadores) (car operandos) (cadr operandos))
                               (cddr operandos)))))

  (apply-operators lrators lrands))

-----------------------------------------------------------------------------------------------

#lang eopl

#|
   Taller 1 FLP
   Ervin Caravali Ibarra 1925648
   Jhoimar Enrique Silva 2177167
   Estiven Andres Martinez G 2179687
|#

;;(Operar-binarias operacionB)
;; 16.
;;Proposito
;; (operador)x(operando1)x(operando2)->(operacionB):La función operate realiza una serie de cálculos binarios sucesivos según los operadores y operandos proporcionados.
;; 
;;Gramática BNF:
;;<OperacionB>::= <int>
;;::= (<OperacionB> ’suma <OperacionB>)
;;::= (<OperacionB> ’resta <OperacionB>)
;;::= (<OperacionB> ’multiplica <OperacionB>)
;;
;;
;;Pruebas
;;(display (Operar-binarias 4)) ; Debe imprimir 4
;;(display (Operar-binarias '(2 suma 9))) ; Debe imprimir 11
;;(display (Operar-binarias '(2 resta 9))) ; Debe imprimir -7
;;(display (Operar-binarias '(2 multiplica 9))) ; Debe imprimir 18
;;(display (Operar-binarias '(6 resta (2 suma (3 multiplica 4)))));Debe imprimir -8
;;(display (Operar-binarias '(3 resta (4 suma 2)))) ; Debe imprimir -3
;;(display (Operar-binarias '((2 multiplica 3) suma (5 resta 1))))
;;(display (Operar-binarias '((2 multiplica (4 suma 1))multiplica((2 multiplica 4) resta 1))))


(define (Operar-binarias operacionB)
  (cond
    ((number? operacionB) operacionB) ; Caso base: si es un número, se retorna el número
    ((list? operacionB)
     (let* ((operador (cadr operacionB))
            (operando1 (Operar-binarias (car operacionB)))
            (operando2 (Operar-binarias (caddr operacionB))))
       (cond
         ((eq? operador 'suma) (+ operando1 operando2))
         ((eq? operador 'resta) (- operando1 operando2))
         ((eq? operador 'multiplica) (* operando1 operando2))
         (else ("Operador no válido")))))
    (else ("Operación binaria no válida"))))





