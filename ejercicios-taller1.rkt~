#lang eopl
#|
 Estiven Andrés Martínez Granados:2179687
 Jhoimar Silva Torres:2177167
 Ervin Caravali Ibarra:1925648
|#

#|
Punto 3

a.)las expresines BNF:

<list-set>          ::= list-set(<lista> <numero> <elemento>)
<lista>             ::=  <elemento> <resto-de-la-lista> 
<resto-de-la-lista> ::=  <elemento> <resto-de-la-lista> | ε
<elemento>          ::= cualquier elemento de la lista
<numero>            ::= un número entero que representa el índice

b.)  list-set:
     Propocito:
     L x N x X -> L’ : Procedimiento que reemplaza el elemento en la posición N de la lista L con X.

     <lista> := ()
     <lista> ::= <elemento> <resto-de-la-lista> 
             ::= (<valor-de-scheme> <lista>) 

c.) casos de prueba:

    (list-set '() 0 'x)
    ()
    (list-set '(a b c) 5 'x)
    '(a b c)
    (list-set '(1 2 3) 0 'x)
    '(x 2 3)
    (list-set '(1 2 3 4) 2 'x)
    '(1 2 x 4)
    (list-set '(a b c) 2 'x)
    '(a b x)

|#

(define list-set
  (lambda (L n x)
    (if (null? L)
        '()
        (if (= n 0)
            (cons x (cdr L))
            (cons (car L) (list-set (cdr L) (- n 1) x))))))








