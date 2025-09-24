#lang racket

(require "logica.rkt")

(provide calcular-minas-adyacentes
         descubrir-vecinas)

;; ---------------------------------------------
;; 1. Calcular minas adyacentes (8 vecinos)
;; ---------------------------------------------

(define (calcular-minas-adyacentes tablero fila col)
  (define vecinos
    (list (list -1 -1) (list -1 0) (list -1 1)
          (list 0 -1)             (list 0 1)
          (list 1 -1)  (list 1 0) (list 1 1)))
  (contar-minas-en-lista tablero fila col vecinos))

;; Función recursiva auxiliar
(define (contar-minas-en-lista tablero fila col vecinos)
  (cond [(null? vecinos) 0]
        [else
         (define dx (car (car vecinos)))
         (define dy (cadr (car vecinos)))
         (define valor (obtener-valor-casilla tablero (+ fila dx) (+ col dy)))
         (+ (if (and valor (= valor 1)) 1 0)
            (contar-minas-en-lista tablero fila col (cdr vecinos)))]))

;; ---------------------------------------------
;; 2. Descubrir vecinas recursivamente
;;    (flood-fill cuando hay 0 minas adyacentes)
;; ---------------------------------------------

(define (descubrir-vecinas tablero fila col)
  ;; obtenemos minas alrededor
  (define minas (calcular-minas-adyacentes tablero fila col))
  (cond
    ;; caso: hay minas alrededor -> actualizar casilla con el número
    [(> minas 0)
     (actualizar-casilla tablero fila col minas)]
    ;; caso: no hay minas alrededor -> marcar 0 y expandir vecinos
    [else
     (expandir-vecinos (actualizar-casilla tablero fila col 0)
                       fila col
                       (list (list -1 -1) (list -1 0) (list -1 1)
                             (list 0 -1)             (list 0 1)
                             (list 1 -1)  (list 1 0) (list 1 1)))]))

;; expandir sobre lista de vecinos
(define (expandir-vecinos tablero fila col vecinos)
  (cond [(null? vecinos) tablero]
        [else
         (define dx (car (car vecinos)))
         (define dy (cadr (car vecinos)))
         (define new-fila (+ fila dx))
         (define new-col (+ col dy))
         (define valor (obtener-valor-casilla tablero new-fila new-col))
         (cond
           ;; fuera de rango o ya descubierta
           [(or (not valor) (number? valor))
            (expandir-vecinos tablero fila col (cdr vecinos))]
           ;; si es mina, no expandir
           [(= valor 1)
            (expandir-vecinos tablero fila col (cdr vecinos))]
           ;; si es vacía (0), descubrir recursivamente
           [else
            (define nuevo-tablero (descubrir-vecinas tablero new-fila new-col))
            (expandir-vecinos nuevo-tablero fila col (cdr vecinos))])]))
