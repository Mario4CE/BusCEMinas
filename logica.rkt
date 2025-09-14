#lang racket

; Exportar las funciones que quieres usar en otros archivos
(provide calcular-minas 
         crear-tablero
         obtener-valor-casilla
         actualizar-casilla
         obtener-fila
         contar-elementos
         recorrer-tablero)

; Función para calcular las minas según dificultad
; 'facil -> 10% de las minas 
; 'medio -> 15% de las minas
; 'dificil -> 20% de las minas
(define (calcular-minas ancho alto dificultad)
  (define total-casillas (* ancho alto))
  (case dificultad
    [(facil) (inexact->exact (ceiling (* total-casillas 0.10)))]
    [(medio) (inexact->exact (ceiling (* total-casillas 0.15)))]
    [(dificil) (inexact->exact (ceiling (* total-casillas 0.20)))]
    [else 10]))

; Esta funcion crea, recursivamente, una lista '() de listas '(())
; de tamaño ancho x alto, inicializada con ceros
; 0 = vacio, 1 = mina
(define (crear-tablero ancho alto)
    (cond [(= alto 0) '()] ; Caso base: si el alto es 0, retorna lista vacia
          [else (cons (crear-fila ancho) (crear-tablero ancho (- alto 1)))]))

(define (crear-fila ancho)
    (make-list ancho 0)) ; Crea una fila de ancho inicializada con ceros

; Función recursiva para obtener el valor de una casilla específica (fila, columna)
; usando programación funcional con car y cdr
(define (obtener-valor-casilla tablero fila columna)
  (cond [(or (< fila 0) (< columna 0) (null? tablero)) #f] ; Caso base: fuera de límites
        [(= fila 0) (obtener-elemento-fila (car tablero) columna)] ; Si estamos en la fila correcta
        [else (obtener-valor-casilla (cdr tablero) (- fila 1) columna)])) ; Recorrer recursivamente

; Función recursiva para obtener un elemento de una fila específica
(define (obtener-elemento-fila fila columna)
  (cond [(or (< columna 0) (null? fila)) #f] ; Caso base: fuera de límites
        [(= columna 0) (car fila)] ; Si estamos en la columna correcta
        [else (obtener-elemento-fila (cdr fila) (- columna 1))])) ; Recorrer recursivamente

; Función para actualizar una casilla en el tablero (devuelve nuevo tablero)
; Usando programación funcional inmutable
(define (actualizar-casilla tablero fila columna nuevo-valor)
  (cond [(or (< fila 0) (null? tablero)) tablero] ; Caso base
        [(= fila 0) (cons (actualizar-fila (car tablero) columna nuevo-valor) 
                         (cdr tablero))] ; Actualizar la fila correcta
        [else (cons (car tablero) 
                   (actualizar-casilla (cdr tablero) (- fila 1) columna nuevo-valor))])) ; Recorrer recursivamente

; Función auxiliar para actualizar una fila
(define (actualizar-fila fila columna nuevo-valor)
  (cond [(or (< columna 0) (null? fila)) fila] ; Caso base
        [(= columna 0) (cons nuevo-valor (cdr fila))] ; Reemplazar el elemento
        [else (cons (car fila) 
                   (actualizar-fila (cdr fila) (- columna 1) nuevo-valor))])) ; Recorrer recursivamente

; Función recursiva para obtener una fila específica del tablero
(define (obtener-fila tablero indice-fila)
  (cond [(or (< indice-fila 0) (null? tablero)) '()] ; Caso base
        [(= indice-fila 0) (car tablero)] ; Fila encontrada
        [else (obtener-fila (cdr tablero) (- indice-fila 1))])) ; Recorrer recursivamente

; Función recursiva para contar elementos en una lista
(define (contar-elementos lista)
  (cond [(null? lista) 0] ; Caso base
        [else (+ 1 (contar-elementos (cdr lista)))])) ; Contar recursivamente

; Función recursiva para recorrer el tablero y aplicar una función a cada elemento
(define (recorrer-tablero tablero funcion fila)
  (cond [(null? tablero) '()] ; Caso base
        [else (cons (recorrer-fila (car tablero) funcion fila 0)
                   (recorrer-tablero (cdr tablero) funcion (+ fila 1)))])) ; Recorrer recursivamente

; Función auxiliar para recorrer una fila
(define (recorrer-fila fila funcion indice-fila columna)
  (cond [(null? fila) '()] ; Caso base
        [else (cons (funcion (car fila) indice-fila columna)
                   (recorrer-fila (cdr fila) funcion indice-fila (+ columna 1)))])) ; Recorrer recursivamente    
