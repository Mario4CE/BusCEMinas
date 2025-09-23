#lang racket
(require racket/random)

; Exportar las funciones necesarias
(provide lista1 barajar)

(define (lista1 total-casillas NumMinas)
  (cond ((= total-casillas 0) '()) ; caso base: lista vacía
    ((> NumMinas 0) (cons 1 (lista1 (- total-casillas 1) (- NumMinas 1)))) ; agrega 1
    (else (cons 0 (lista1 (- total-casillas 1) NumMinas))))) ; agrega 0

;; Quita elemento en posición indice y devuelve dos valores:
;;   - el elemento escogido
;;   - la lista restante
(define (quitar-en-posicion lista indice)
  (cond
    ((zero? indice) (values (car lista) (cdr lista)))
    (else
     (define-values (elemento lista-restante) (quitar-en-posicion (cdr lista) (- indice 1)))
     (values elemento (cons (car lista) lista-restante)))))

;; Algoritmo Fisher-Yates funcional recursivo inspirado en la version de Amit Saha
(define (barajar lista numero-elementos)
  (cond
    ((<= numero-elementos 1) lista) ; caso base
    (else
     (define indice (random numero-elementos)) ; índice aleatorio entre 0 y numero-elementos-1, este random es para que siempre sean permutaciones diferentes
     (define-values (seleccionado resto) (quitar-en-posicion lista indice)) ; separar
     (cons seleccionado (barajar resto (- numero-elementos 1)))))) ; reconstrucción recursiva
