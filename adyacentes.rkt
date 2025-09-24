#lang racket
(require "logica.rkt")

(provide calcular-minas-adyacentes
         descubrir-vecinas)

(define (calcular-minas-adyacentes tablero fila col)
  (unless (list? tablero)
    (error "Error: tablero debe ser una lista, recibido: ~a" tablero))
  (define vecinos
    (list (list -1 -1) (list -1 0) (list -1 1)
          (list 0 -1)             (list 0 1)
          (list 1 -1)  (list 1 0) (list 1 1)))
  (contar-minas-en-lista tablero fila col vecinos))

(define (contar-minas-en-lista tablero fila col vecinos)
  (unless (list? vecinos)
    (error "Error: vecinos debe ser una lista, recibido: ~a" vecinos))
  (cond [(null? vecinos) 0]
        [else
         (define dx (car (car vecinos)))
         (define dy (cadr (car vecinos)))
         (define valor (obtener-valor-casilla tablero (+ fila dx) (+ col dy)))
         (+ (if (and valor (= valor 1)) 1 0)
            (contar-minas-en-lista tablero fila col (cdr vecinos)))]))

(define (descubrir-vecinas tablero fila col)
  (define minas (calcular-minas-adyacentes tablero fila col))
  (cond
    [(> minas 0)
     (actualizar-casilla tablero fila col minas)]
    [else
     (expandir-vecinos (actualizar-casilla tablero fila col 0)
                       fila col
                       (list (list -1 -1) (list -1 0) (list -1 1)
                             (list 0 -1)             (list 0 1)
                             (list 1 -1)  (list 1 0) (list 1 1)))]))

(define (expandir-vecinos tablero fila col vecinos)
  (cond [(null? vecinos) tablero]
        [else
         (define dx (car (car vecinos)))
         (define dy (cadr (car vecinos)))
         (define new-fila (+ fila dx))
         (define new-col (+ col dy))
         (define valor (obtener-valor-casilla tablero new-fila new-col))
         (cond
           [(or (not valor) (number? valor))
            (expandir-vecinos tablero fila col (cdr vecinos))]
           [(= valor 1)
            (expandir-vecinos tablero fila col (cdr vecinos))]
           [else
            (define nuevo-tablero (descubrir-vecinas tablero new-fila new-col))
            (expandir-vecinos nuevo-tablero fila col (cdr vecinos))])]))
