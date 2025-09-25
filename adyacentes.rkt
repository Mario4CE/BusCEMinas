#lang racket
(require "logica.rkt")

(provide calcular-minas-adyacentes
         descubrir-vecinas-extendido)

;; Calcular minas alrededor (distancia 1)
(define (calcular-minas-adyacentes tablero fila col)
  (define vecinos
    (for*/list ([dx '(-1 0 1)]
                [dy '(-1 0 1)]
                #:when (not (and (= dx 0) (= dy 0))))
      (list dx dy)))
  (foldl (λ (offset acc)
           (define dx (car offset))
           (define dy (cadr offset))
           (define valor (obtener-valor-casilla tablero (+ fila dx) (+ col dy)))
           (+ acc (if (and valor (= valor 1)) 1 0)))
         0
         vecinos))

;; Descubrir una casilla y expandir vecinos si es 0
;; es-casilla-inicial? evita que se ignore la casilla inicial aunque ya esté descubierta
(define (descubrir-vecinas-extendido tablero-visual tablero fila col ancho alto lista-barajada casillas-descubiertas-ref
                                     #:es-casilla-inicial? [es-casilla-inicial? #f])
  (displayln (format "Procesando casilla (~a,~a), inicial=~a" fila col es-casilla-inicial?))
  (if (or (< fila 0) (>= fila alto) (< col 0) (>= col ancho))
      (begin
        (displayln (format "Casilla (~a,~a) fuera de límites, ignorando" fila col))
        tablero)
      (let ([valor (obtener-valor-casilla lista-barajada fila col)]
            [minas (calcular-minas-adyacentes lista-barajada fila col)]
            [boton (list-ref (list-ref tablero-visual fila) col)])
        (displayln (format "Casilla (~a,~a): valor=~a, minas adyacentes=~a, tablero lógico=~a"
                           fila col valor minas (obtener-valor-casilla tablero fila col)))
        (cond
          [(not valor)
           (displayln (format "Casilla (~a,~a) fuera de rango, ignorando" fila col))
           tablero]
          [(= valor 1)
           (displayln (format "Casilla (~a,~a) es una mina, ignorando" fila col))
           tablero]
          [(and (not es-casilla-inicial?) (number? (obtener-valor-casilla tablero fila col)))
           (displayln (format "Casilla (~a,~a) ya descubierta, ignorando" fila col))
           tablero]
          [(> minas 0)
           (displayln (format "Marcando (~a,~a) con número: ~a" fila col minas))
           (send boton set-label (number->string minas))
           (send boton enable #f)
           (set-box! casillas-descubiertas-ref (add1 (unbox casillas-descubiertas-ref)))
           (actualizar-casilla tablero fila col minas)]
          [else
           (displayln (format "Marcando (~a,~a) como vacío (0) y expandiendo vecinos" fila col))
           (send boton set-label "")
           (send boton enable #f)
           (set-box! casillas-descubiertas-ref (add1 (unbox casillas-descubiertas-ref)))
           (expandir-vecinos-extendido
            tablero-visual
            (actualizar-casilla tablero fila col 0)
            fila col
            (generar-vecinos-extendidos)
            ancho alto lista-barajada casillas-descubiertas-ref)]))))

;; Generar offsets para vecinos (distancia 1)
(define (generar-vecinos-extendidos) ; devuelve lista de (dx dy)
  (for*/list ([dx '(-1 0 1)]
              [dy '(-1 0 1)]
              #:when (not (and (= dx 0) (= dy 0))))
    (list dx dy)))

;; Expansión funcional de vecinos
(define (expandir-vecinos-extendido tablero-visual tablero fila col vecinos ancho alto lista-barajada casillas-descubiertas-ref)
  (cond
    [(null? vecinos) tablero]
    [else
     (define dx (caar vecinos))
     (define dy (cadar vecinos))
     (define new-fila (+ fila dx))
     (define new-col (+ col dy))
     (displayln (format "Evaluando vecino (~a,~a)" new-fila new-col))
     (define nuevo-tablero
       (descubrir-vecinas-extendido tablero-visual tablero new-fila new-col ancho alto lista-barajada casillas-descubiertas-ref
                                    #:es-casilla-inicial? #f))
     (expandir-vecinos-extendido tablero-visual nuevo-tablero fila col (cdr vecinos) ancho alto lista-barajada casillas-descubiertas-ref)]))