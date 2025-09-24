#lang racket
(require "adyacentes.rkt"
         "logica.rkt"
         racket/gui/base)

(provide descubrir-vecinas-gui)

(define (descubrir-vecinas-gui fila columna tablero-visual tablero-logico ancho alto lista-barajada casillas-descubiertas-ref)
  (displayln (format "Procesando casilla (~a, ~a)" fila columna))
  (displayln (format "Lista barajada: ~a" lista-barajada))
  (unless (list? lista-barajada)
    (error "Error: lista-barajada debe ser una lista, recibido: ~a" lista-barajada))
  (if (or (< fila 0) (>= fila alto) (< columna 0) (>= columna ancho))
      (begin
        (displayln (format "Casilla (~a, ~a) fuera de límites, omitiendo" fila columna))
        tablero-logico)
      (let ([boton (list-ref (list-ref tablero-visual fila) columna)])
        (if (send boton is-enabled?)
            (let ([minas (calcular-minas-adyacentes lista-barajada fila columna)])
              (displayln (format "Minas en (~a, ~a): ~a" fila columna minas))
              (let ([nuevo-tablero
                     (cond
                       [(> minas 0)
                        (send boton set-label (number->string minas))
                        (send boton enable #f)
                        (set-box! casillas-descubiertas-ref
                                  (add1 (unbox casillas-descubiertas-ref)))
                        (actualizar-casilla tablero-logico fila columna minas)]
                       [else
                        (send boton set-label "")
                        (send boton enable #f)
                        (set-box! casillas-descubiertas-ref
                                  (add1 (unbox casillas-descubiertas-ref)))
                        (let ([tablero-actual (actualizar-casilla tablero-logico fila columna 0)])
                          (for/fold ([tablero-actual tablero-actual])
                                    ([dx '(-1 0 1)]
                                     [dy '(-1 0 1)]
                                     #:when (not (and (= dx 0) (= dy 0))))
                                    (let ([nf (+ fila dx)]
                                          [nc (+ columna dy)])
                                      (if (and (>= nf 0) (< nf alto)
                                               (>= nc 0) (< nc ancho))
                                          (let ([vecino (list-ref (list-ref tablero-visual nf) nc)])
                                            (if (send vecino is-enabled?)
                                                (begin
                                                  (displayln (format "Explorando vecino (~a, ~a)" nf nc))
                                                  (descubrir-vecinas-gui
                                                   nf nc tablero-visual tablero-actual ancho alto lista-barajada casillas-descubiertas-ref))
                                                tablero-actual))
                                          tablero-actual))))])])
                nuevo-tablero))
            tablero-logico))))
