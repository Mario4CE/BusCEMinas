#lang racket
(require "logica.rkt")

(provide calcular-minas-adyacentes
         descubrir-vecinas-extendido)

;; ============================================================
;; Generar offsets de vecinos (distancia 1, excluyendo (0,0))
;; ============================================================

(define (generar-vecinos-extendidos)
  (generar-vecinos-aux '(-1 0 1) '(-1 0 1)))

(define (generar-vecinos-aux dxs dys)
  (cond
    [(null? dxs) '()]
    [else
     (append (generar-vecinos-dy (car dxs) dys)
             (generar-vecinos-aux (cdr dxs) dys))]))

(define (generar-vecinos-dy dx dys)
  (cond
    [(null? dys) '()]
    [(and (= dx 0) (= (car dys) 0))
     (generar-vecinos-dy dx (cdr dys))] ; saltar (0,0)
    [else
     (cons (list dx (car dys))
           (generar-vecinos-dy dx (cdr dys)))]))

;; ============================================================
;; Calcular minas adyacentes
;; ============================================================

(define (calcular-minas-adyacentes tablero fila col)
  (calcular-minas-adyacentes-aux tablero fila col (generar-vecinos-extendidos)))

(define (calcular-minas-adyacentes-aux tablero fila col vecinos)
  (cond
    [(null? vecinos) 0]
    [else
     (+ (if (let ([valor (obtener-valor-casilla tablero
                                               (+ fila (caar vecinos))
                                               (+ col (cadar vecinos)))])
              (and valor (= valor 1)))
            1
            0)
        (calcular-minas-adyacentes-aux tablero fila col (cdr vecinos)))]))

;; ============================================================
;; Descubrir casillas y expandir si es 0
;; ============================================================

(define (descubrir-vecinas-extendido tablero-visual tablero fila col ancho alto lista-barajada casillas-descubiertas-ref
                                     #:es-casilla-inicial? [es-casilla-inicial? #f])
  (if (or (< fila 0) (>= fila alto) (< col 0) (>= col ancho))
      tablero
      (let ([valor (obtener-valor-casilla lista-barajada fila col)]
            [minas (calcular-minas-adyacentes lista-barajada fila col)]
            [boton (list-ref (list-ref tablero-visual fila) col)])
        (cond
          [(not valor) tablero]
          [(= valor 1) tablero]
          [(and (not es-casilla-inicial?)
                (number? (obtener-valor-casilla tablero fila col)))
           tablero]
          [(> minas 0)
           (send boton set-label (number->string minas))
           (send boton enable #f)
           (set-box! casillas-descubiertas-ref (add1 (unbox casillas-descubiertas-ref)))
           (actualizar-casilla tablero fila col minas)]
          [else
           (send boton set-label "")
           (send boton enable #f)
           (set-box! casillas-descubiertas-ref (add1 (unbox casillas-descubiertas-ref)))
           (expandir-vecinos-extendido
            tablero-visual
            (actualizar-casilla tablero fila col 0)
            fila col
            (generar-vecinos-extendidos)
            ancho alto lista-barajada casillas-descubiertas-ref)]))))

;; ============================================================
;; Expansión funcional de vecinos (pura recursión)
;; ============================================================

(define (expandir-vecinos-extendido tablero-visual tablero fila col vecinos ancho alto lista-barajada casillas-descubiertas-ref)
  (cond
    [(null? vecinos) tablero]
    [else
     (expandir-vecinos-extendido
      tablero-visual
      (descubrir-vecinas-extendido
       tablero-visual
       tablero
       (+ fila (caar vecinos))
       (+ col (cadar vecinos))
       ancho alto lista-barajada casillas-descubiertas-ref
       #:es-casilla-inicial? #f)
      fila col
      (cdr vecinos)
      ancho alto lista-barajada casillas-descubiertas-ref)]))
;; ============================================================