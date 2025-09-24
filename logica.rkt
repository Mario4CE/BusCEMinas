#lang racket
(require racket/random)

(provide calcular-minas
         crear-tablero
         obtener-valor-casilla
         actualizar-casilla
         obtener-fila
         contar-elementos
         recorrer-tablero
         generar-lista-barajada)

; Calcular minas según dificultad
(define (calcular-minas ancho alto dificultad)
  (define total-casillas (* ancho alto))
  (case dificultad
    [(facil) (inexact->exact (ceiling (* total-casillas 0.10)))]
    [(medio) (inexact->exact (ceiling (* total-casillas 0.15)))]
    [(dificil) (inexact->exact (ceiling (* total-casillas 0.20)))]
    [else 10]))

; Crear tablero (matriz de ceros)
(define (crear-tablero ancho alto)
  (cond [(= alto 0) '()]
        [else (cons (crear-fila ancho) (crear-tablero ancho (- alto 1)))]))

(define (crear-fila ancho)
  (make-list ancho 0))

; Obtener valor de una casilla
(define (obtener-valor-casilla tablero fila columna)
  (cond [(or (< fila 0) (< columna 0) (null? tablero)) #f]
        [(= fila 0) (obtener-elemento-fila (car tablero) columna)]
        [else (obtener-valor-casilla (cdr tablero) (- fila 1) columna)]))

(define (obtener-elemento-fila fila columna)
  (cond [(or (< columna 0) (null? fila)) #f]
        [(= columna 0) (car fila)]
        [else (obtener-elemento-fila (cdr fila) (- columna 1))]))

; Actualizar una casilla
(define (actualizar-casilla tablero fila columna nuevo-valor)
  (cond [(or (< fila 0) (null? tablero)) tablero]
        [(= fila 0) (cons (actualizar-fila (car tablero) columna nuevo-valor)
                          (cdr tablero))]
        [else (cons (car tablero)
                    (actualizar-casilla (cdr tablero) (- fila 1) columna nuevo-valor))]))

(define (actualizar-fila fila columna nuevo-valor)
  (cond [(or (< columna 0) (null? fila)) fila]
        [(= columna 0) (cons nuevo-valor (cdr fila))]
        [else (cons (car fila)
                    (actualizar-fila (cdr fila) (- columna 1) nuevo-valor))]))

; Obtener una fila
(define (obtener-fila tablero indice-fila)
  (cond [(or (< indice-fila 0) (null? tablero)) '()]
        [(= indice-fila 0) (car tablero)]
        [else (obtener-fila (cdr tablero) (- indice-fila 1))]))

; Contar elementos
(define (contar-elementos lista)
  (cond [(null? lista) 0]
        [else (+ 1 (contar-elementos (cdr lista)))]))

; Recorrer tablero
(define (recorrer-tablero tablero funcion fila)
  (cond [(null? tablero) '()]
        [else (cons (recorrer-fila (car tablero) funcion fila 0)
                    (recorrer-tablero (cdr tablero) funcion (+ fila 1)))]))

(define (recorrer-fila fila funcion indice-fila columna)
  (cond [(null? fila) '()]
        [else (cons (funcion (car fila) indice-fila columna)
                    (recorrer-fila (cdr fila) funcion indice-fila (+ columna 1)))]))

; Generar lista-barajada como matriz
(define (generar-lista-barajada ancho alto num-minas)
  (unless (and (> ancho 0) (> alto 0) (>= num-minas 0))
    (error "Error: ancho, alto deben ser > 0, num-minas >= 0"))
  (let* ([total-casillas (* ancho alto)]
         [lista-plana (barajar (lista1 total-casillas num-minas) total-casillas)])
    (list->matriz lista-plana ancho alto)))

(define (lista1 total-casillas NumMinas)
  (unless (>= total-casillas NumMinas)
    (error "Error: total-casillas debe ser >= NumMinas"))
  (cond ((= total-casillas 0) '())
        ((> NumMinas 0) (cons 1 (lista1 (- total-casillas 1) (- NumMinas 1))))
        (else (cons 0 (lista1 (- total-casillas 1) NumMinas)))))

(define (quitar-en-posicion lista indice)
  (cond
    ((zero? indice) (values (car lista) (cdr lista)))
    (else
     (define-values (elemento lista-restante) (quitar-en-posicion (cdr lista) (- indice 1)))
     (values elemento (cons (car lista) lista-restante)))))

(define (barajar lista numero-elementos)
  (cond
    ((<= numero-elementos 1) lista)
    (else
     (define indice (random numero-elementos))
     (define-values (seleccionado resto) (quitar-en-posicion lista indice))
     (cons seleccionado (barajar resto (- numero-elementos 1))))))

(define (list->matriz lista ancho alto)
  (if (or (null? lista) (= alto 0))
      '()
      (cons (take lista ancho)
            (list->matriz (drop lista ancho) ancho (- alto 1)))))