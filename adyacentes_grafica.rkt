#lang racket
(require "adyacentes.rkt"
         "logica.rkt")

(provide descubrir-vecinas-gui)

;; --------------------------------------------------------
;; Función que traduce la lógica funcional de adyacentes
;; al tablero visual (botones de la GUI)
;; --------------------------------------------------------

(define (descubrir-vecinas-gui fila columna tablero-visual ancho alto lista-barajada casillas-descubiertas-ref)
  ;; tablero-visual: matriz de botones (GUI)
  ;; ancho, alto: dimensiones del tablero
  ;; lista-barajada: lista con minas (0/1)
  ;; casillas-descubiertas-ref: caja mutable (box) para contador en GUI

  (define boton (list-ref (list-ref tablero-visual fila) columna))

  (when (send boton is-enabled?)
    (define minas (calcular-minas-adyacentes lista-barajada fila columna))
    (cond
      ;; Si hay minas alrededor, solo mostrar número
      [(> minas 0)
       (send boton set-label (number->string minas))
       (send boton enable #f)
       (set-box! casillas-descubiertas-ref (add1 (unbox casillas-descubiertas-ref)))]

      ;; Si no hay minas alrededor, expandir
      [else
       (send boton set-label "")
       (send boton enable #f)
       (set-box! casillas-descubiertas-ref (add1 (unbox casillas-descubiertas-ref)))
       ;; Expandir a los 8 vecinos
       (for ([dx '(-1 0 1)]
             [dy '(-1 0 1)]
             #:when (not (and (= dx 0) (= dy 0))))
         (define nf (+ fila dx))
         (define nc (+ columna dy))
         (when (and (>= nf 0) (< nf alto)
                    (>= nc 0) (< nc ancho))
           (descubrir-vecinas-gui nf nc tablero-visual ancho alto lista-barajada casillas-descubiertas-ref))))]))
