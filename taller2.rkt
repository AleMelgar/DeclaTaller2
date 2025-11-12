#lang racket

;; Alejandro Jose Melgar Deleon 00026222
;; Taller 2 - Programacion Declarativa

;; ============================
;; Ejercicio 1 – Contar elementos positivos
(define (contar-positivos lst)
  (length (filter (lambda (x) (> x 0)) lst)))

;; ============================
;; Ejercicio 2 – Lista de cuadrados pares
(define (cuadrados-pares lst)
  (map (lambda (x) (* x x))
       (filter even? lst)))

;; ============================
;; Ejercicio 3 – Factorial recursivo
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; ============================
;; Ejercicio 4 – Elevar cada número al cubo
(define (cubos lst)
  (map (lambda (x) (* x x x)) lst))

;; ============================
;; Ejercicio 5 – Sumar impares
(define (suma-impares lst)
  (foldl + 0 (filter odd? lst)))

;; ============================
;; Ejercicio 6 – Contiene negativos
(define (contiene-negativos? lst)
  (ormap (lambda (x) (< x 0)) lst))

;; ============================
;; Ejercicio 7 – Suma acumulada
(define (suma-acumulada lst)
  (foldl (lambda (x acc)
           (append acc (list (+ x (if (null? acc) 0 (last acc))))))
         '()
         lst))

;; ============================
;; Ejercicio 8 – Concatenar cadenas
(define (concat-cadenas lst)
  (foldr string-append "" lst))


;; ============================
;; Ejercicio 9 – Dobles de números > 5
(define (dobles-mayores-a5 lst)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lst)))

;; ============================
;; Ejercicio 10 – Invertir lista
(define (invertir lst)
  (foldl (lambda (x acc) (cons x acc)) '() lst))

;; ============================
;; Ejercicio 11 – Función como parámetro
(define (aplicar-a-lista f lst)
  (map f lst))

;; ============================
;; Ejercicio 12 – Promedio de mayores a 5
(define (promedio-mayores-a5 lst)
  (let* ([mayores (filter (lambda (x) (> x 5)) lst)]
         [suma (foldl + 0 mayores)]
         [cantidad (length mayores)])
    (/ suma cantidad)))


;; ============================
;; PRUEBAS (entradas del documento)

;; Ejercicio 1
(contar-positivos '(3 -2 7 0 -5 9))

;; Ejercicio 2
(cuadrados-pares '(1 2 3 4 5 6 7 8))

;; Ejercicio 3
(factorial 5)

;; Ejercicio 4
(cubos '(2 3 4))

;; Ejercicio 5
(suma-impares '(1 2 3 4 5 6 7))

;; Ejercicio 6
(contiene-negativos? '(5 9 -3 2))

;; Ejercicio 7
(suma-acumulada '(1 2 3 4))

;; Ejercicio 8
(concat-cadenas '("Hola" " " "Mundo"))

;; Ejercicio 9
(dobles-mayores-a5 '(3 6 8 2 10))

;; Ejercicio 10
(invertir '(1 2 3 4))

;; Ejercicio 11
(aplicar-a-lista (lambda (x) (* x x)) '(1 2 3 4))

;; Ejercicio 12
(promedio-mayores-a5 '(3 8 10 4 9 2 7))
