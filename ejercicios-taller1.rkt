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
#|

Punto 6
a.)las expresines BNF:
           
           funcion auxiliar my-map
           <my-map> ::= my-map(<func> <lista>)
           <lista>  ::= <elemento> <resto-de-la-lista>
<resto-de-la-lista> ::= <elemento> <resto-de-la-lista> | ε
           <func>   ::= cualquier función
         <elemento> ::= cualquier elemento de la lista

           funcion swapper

            <swapper> ::= swapper(<elemento> <elemento> <lista>)
            <lista>   ::= <elemento> <resto-de-la-lista> 
  <resto-de-la-lista> ::= <elemento> <resto-de-la-lista> | ε
           <elemento> ::= cualquier elemento de la lista

b.)     my-map:
        Propocito:
        F x L -> L’ :Aplica la función (F) a cada elemento de la lista lst (L), devolviendo una nueva lista (L') con los resultados.
         
        swapper:
        Propocito:
        E1 E2 L -> L’ : reemplaza todas las ocurrencias de E1 por E2 y viceversa en la lista L, devolviendo una nueva lista con los reemplazos realizados.

c.) casos de prueba:
    
    (my-map (lambda (x) (* x x)) '(1 2 3 4 5))
    (1 4 9 16 25)
    
    (my-map (lambda (x) (+ x 10)) '(10 20 30 40 50))
    (20 30 40 50 60)
    
    (my-map (lambda (x) (string-append "Hola " x)) '("Juan" "Maria" "Pedro"))
    ( "Hola Juan" "Hola Maria" "Hola Pedro")
    
    (my-map (lambda (x) (if (even? x) 'par 'impar)) '(1 2 3 4 5))
    (impar par impar par impar)
    
    (my-map (lambda (x) (if (null? x) '() (car x))) '((1 2) (3 4) (5 6)))
    (1 3 5)

   
    (swapper 'a 'b '(a b c a b a))
    '(b a c b a b)

    (swapper 'x 'y '(x y x x y x))
   '(y x y y x y)

    (swapper 1 2 '(1 2 3 4 5))
   '(2 1 3 4 5)

    (swapper 'a 'b '(c d e f))
   '(c d e f)

    (swapper 'a 'b '())
   '()
|#


(define (my-map func lst)
  (if (null? lst)
      '()
      (cons ((lambda (x) (func x)) (car lst))
            (my-map func (cdr lst)))))


(define (swapper E1 E2 L)
  (define (swap elem)
    ((lambda (x) (cond ((equal? x E1) E2)
                       ((equal? x E2) E1)
                       (else x))) elem))
  (my-map swap L))






