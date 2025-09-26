#lang racket
(require racket/random
         racket/list)

(provide calcular-minas
         crear-tablero
         obtener-valor-casilla
         actualizar-casilla
         obtener-fila
         contar-elementos
         recorrer-tablero
         generar-lista-barajada)

;; -------------------------------
;; Calcular minas según dificultad
;; -------------------------------
(define (calcular-minas ancho alto dificultad)
  (define total-casillas (* ancho alto))
  (case dificultad
    [(facil)   (inexact->exact (ceiling (* total-casillas 0.10)))] ; 10%
    [(medio)   (inexact->exact (ceiling (* total-casillas 0.15)))] ; 15%
    [(dificil) (inexact->exact (ceiling (* total-casillas 0.20)))] ; 20%
    [else 10]))

;; -------------------------------
;; Crear tablero vacío (#f) con recursión
;; -------------------------------
(define (crear-tablero ancho alto)
  (if (zero? alto)
      '()
      (cons (crear-fila ancho)
            (crear-tablero ancho (sub1 alto)))))

(define (crear-fila ancho)
  (if (zero? ancho)
      '()
      (cons #f (crear-fila (sub1 ancho)))))

;; -------------------------------
;; Obtener valor de casilla
;; -------------------------------
(define (obtener-valor-casilla tablero fila columna)
  (if (or (< fila 0)
          (< columna 0)
          (>= fila (length tablero))
          (>= columna (length (car tablero))))
      #f
      (list-ref (list-ref tablero fila) columna)))

;; -------------------------------
;; Actualizar casilla
;; -------------------------------
(define (actualizar-casilla tablero fila columna nuevo-valor)
  (cond [(null? tablero) '()]
        [(zero? fila)
         (cons (actualizar-fila (car tablero) columna nuevo-valor)
               (cdr tablero))]
        [else
         (cons (car tablero)
               (actualizar-casilla (cdr tablero) (sub1 fila) columna nuevo-valor))]))

(define (actualizar-fila fila columna nuevo-valor)
  (cond [(null? fila) '()]
        [(zero? columna) (cons nuevo-valor (cdr fila))]
        [else (cons (car fila)
                    (actualizar-fila (cdr fila) (sub1 columna) nuevo-valor))]))

;; -------------------------------
;; Obtener una fila
;; -------------------------------
(define (obtener-fila tablero indice-fila)
  (cond [(null? tablero) '()]
        [(zero? indice-fila) (car tablero)]
        [else (obtener-fila (cdr tablero) (sub1 indice-fila))]))

;; -------------------------------
;; Contar elementos en lista
;; -------------------------------
(define (contar-elementos lista)
  (if (null? lista)
      0
      (add1 (contar-elementos (cdr lista)))))

;; -------------------------------
;; Recorrer tablero (recursivo)
;; -------------------------------
(define (recorrer-tablero tablero funcion fila)
  (if (null? tablero)
      '()
      (cons (recorrer-fila (car tablero) funcion fila 0)
            (recorrer-tablero (cdr tablero) funcion (add1 fila)))))

(define (recorrer-fila fila funcion indice-fila columna)
  (if (null? fila)
      '()
      (cons (funcion (car fila) indice-fila columna)
            (recorrer-fila (cdr fila) funcion indice-fila (add1 columna)))))

;; -------------------------------
;; Generar lista barajada
;; -------------------------------
(define (generar-lista-barajada ancho alto num-minas)
  (when (or (<= ancho 0)
            (<= alto 0)
            (< num-minas 0)
            (> num-minas (* ancho alto)))
    (error 'generar-lista-barajada
           "Argumentos inválidos"))
  (define total (* ancho alto))
  (define lista-plana (barajar (construir-lista total num-minas)))
  (list->matriz lista-plana ancho alto))

;; -------------------------------
;; Construir lista de minas/vacíos
;; -------------------------------
(define (construir-lista total minas)
  (cond [(zero? total) '()]
        [(> minas 0) (cons 1 (construir-lista (sub1 total) (sub1 minas)))]
        [else        (cons 0 (construir-lista (sub1 total) minas))]))

;; -------------------------------
;; Barajar lista (recursivo)
;; -------------------------------
(define (barajar lista)
  (cond [(null? lista) '()]
        [else
         (define idx (random (length lista)))
         (define elem (list-ref lista idx))
         (define resto (quitar-en-posicion lista idx))
         (cons elem (barajar resto))]))

(define (quitar-en-posicion lista indice)
  (cond [(zero? indice) (cdr lista)]
        [else (cons (car lista)
                    (quitar-en-posicion (cdr lista) (sub1 indice)))]))

;; -------------------------------
;; Convertir lista en matriz
;; -------------------------------
(define (list->matriz lista ancho alto)
  (if (zero? alto)
      '()
      (cons (sublist lista 0 ancho)
            (list->matriz (drop lista ancho) ancho (sub1 alto)))))

;; Sublista recursiva (sin map/fold)
(define (sublist lista inicio fin)
  (cond [(or (null? lista) (<= fin 0)) '()]
        [(zero? inicio) (cons (car lista)
                              (sublist (cdr lista) 0 (sub1 fin)))]
        [else (sublist (cdr lista) (sub1 inicio) (sub1 fin))]))
;; -------------------------------

