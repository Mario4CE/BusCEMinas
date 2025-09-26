#lang racket
(require "adyacentes.rkt")

(provide descubrir-vecinas-gui)

;; Descubre casillas recursivamente como si se diera clic
(define (descubrir-vecinas-gui fila columna tablero-visual tablero-logico ancho alto lista-barajada casillas-descubiertas-ref)
  (displayln (format "Iniciando expansión en (~a,~a)" fila columna))
  (descubrir-vecinas-extendido tablero-visual tablero-logico fila columna ancho alto lista-barajada casillas-descubiertas-ref
                               #:es-casilla-inicial? #t))