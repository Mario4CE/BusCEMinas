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
; Propósito: Calcula el número de minas en el tablero según el tamaño y la dificultad.
; Entradas:
;   - ancho (entero): Ancho del tablero (número de columnas).
;   - alto (entero): Alto del tablero (número de filas).
;   - dificultad (símbolo): Nivel de dificultad ('facil, 'medio, 'dificil).
; Salida: Un entero que representa el número de minas.
; Ejemplo: (calcular-minas 10 10 'facil) => 10 (10% de 100 casillas).
(define (calcular-minas ancho alto dificultad)
  (define total-casillas (* ancho alto))
  (case dificultad
    [(facil) (inexact->exact (ceiling (* total-casillas 0.10)))]  ; 10% de casillas
    [(medio) (inexact->exact (ceiling (* total-casillas 0.15)))]  ; 15% de casillas
    [(dificil) (inexact->exact (ceiling (* total-casillas 0.20)))]  ; 20% de casillas
    [else 10]))  ; Valor por defecto: 10 minas


; Crear tablero (matriz de #f para casillas no reveladas)
; Propósito: Crea una matriz (lista de listas) que representa el tablero, con todas las casillas inicializadas en #f.
; Entradas:
;   - ancho (entero): Número de columnas del tablero.
;   - alto (entero): Número de filas del tablero.
; Salida: Una lista de listas, donde cada casilla contiene #f (casilla no revelada).
; Ejemplo: (crear-tablero 3 2) => '((#f #f #f) (#f #f #f))
(define (crear-tablero ancho alto)
  (cond [(= alto 0) '()]  ; Si no hay filas, retorna lista vacía
        [else (cons (crear-fila ancho) (crear-tablero ancho (- alto 1)))]))  ; Crea una fila y recursiona

(define (crear-fila ancho)
  ; Propósito: Crea una fila de longitud 'ancho' con todas las casillas en #f.
  ; Entrada: ancho (entero) - Número de columnas en la fila.
  ; Salida: Una lista de 'ancho' elementos, todos #f.
  ; Ejemplo: (crear-fila 3) => '(#f #f #f)
  (make-list ancho #f))  ; Crea lista con 'ancho' elementos #f

; Obtener valor de una casilla
; Propósito: Obtiene el valor de una casilla específica en el tablero.
; Entradas:
;   - tablero (lista de listas): El tablero del juego.
;   - fila (entero): Índice de la fila (0-based).
;   - columna (entero): Índice de la columna (0-based).
; Salida: El valor de la casilla (e.g., #f, número) o #f si está fuera de límites.
; Ejemplo: (obtener-valor-casilla '((#f 1) (2 #f)) 0 1) => 1
(define (obtener-valor-casilla tablero fila columna)
  (displayln (format "Obteniendo valor de casilla (~a,~a), tablero: ~a" fila columna tablero))
  (cond [(or (< fila 0) (< columna 0) (null? tablero))
         (displayln "Fuera de límites, retornando #f")
         #f]  ; Retorna #f si la fila o el tablero son inválidos
        [(= fila 0)
         (let ([valor (obtener-elemento-fila (car tablero) columna)])
           (displayln (format "Valor en (~a,~a): ~a" fila columna valor))
           valor)]  ; Obtiene valor de la primera fila
        [else
         (obtener-valor-casilla (cdr tablero) (- fila 1) columna)]))  ; Recursiona a la siguiente fila

(define (obtener-elemento-fila fila columna)
  ; Propósito: Obtiene el valor de una columna específica en una fila.
  ; Entradas:
  ;   - fila (lista): Una fila del tablero.
  ;   - columna (entero): Índice de la columna (0-based).
  ; Salida: El valor en la columna o #f si está fuera de límites.
  ; Ejemplo: (obtener-elemento-fila '(#f 1 2) 1) => 1
  (cond [(or (< columna 0) (null? fila))
         (displayln "Columna fuera de límites, retornando #f")
         #f]  ; Retorna #f si la columna es inválida
        [(= columna 0)
         (displayln (format "Valor de fila en columna ~a: ~a" columna (car fila)))
         (car fila)]  ; Retorna el primer elemento
        [else
         (obtener-elemento-fila (cdr fila) (- columna 1))]))  ; Recursiona a la siguiente columna

; Actualizar una casilla
; Propósito: Cambia el valor de una casilla específica en el tablero.
; Entradas:
;   - tablero (lista de listas): El tablero del juego.
;   - fila (entero): Índice de la fila (0-based).
;   - columna (entero): Índice de la columna (0-based).
;   - nuevo-valor: El nuevo valor para la casilla (e.g., número, #f).
; Salida: Un nuevo tablero con la casilla actualizada.
; Ejemplo: (actualizar-casilla '((#f #f) (#f #f)) 0 1 1) => '((#f 1) (#f #f))
(define (actualizar-casilla tablero fila columna nuevo-valor)
  (cond [(or (< fila 0) (null? tablero)) tablero]  ; Retorna tablero sin cambios si fila inválida
        [(= fila 0) (cons (actualizar-fila (car tablero) columna nuevo-valor)
                          (cdr tablero))]  ; Actualiza la primera fila
        [else (cons (car tablero)
                    (actualizar-casilla (cdr tablero) (- fila 1) columna nuevo-valor))]))  ; Recursiona

(define (actualizar-fila fila columna nuevo-valor)
  ; Propósito: Actualiza el valor de una columna específica en una fila.
  ; Entradas:
  ;   - fila (lista): Una fila del tablero.
  ;   - columna (entero): Índice de la columna (0-based).
  ;   - nuevo-valor: El nuevo valor para la casilla.
  ; Salida: Una nueva fila con el valor actualizado.
  ; Ejemplo: (actualizar-fila '(#f #f) 1 1) => '(#f 1)
  (cond [(or (< columna 0) (null? fila)) fila]  ; Retorna fila sin cambios si columna inválida
        [(= columna 0) (cons nuevo-valor (cdr fila))]  ; Actualiza el primer elemento
        [else (cons (car fila)
                    (actualizar-fila (cdr fila) (- columna 1) nuevo-valor))]))  ; Recursiona

; Obtener una fila
; Propósito: Extrae una fila específica del tablero.
; Entradas:
;   - tablero (lista de listas): El tablero del juego.
;   - indice-fila (entero): Índice de la fila a extraer (0-based).
; Salida: La fila como una lista, o '() si el índice es inválido.
; Ejemplo: (obtener-fila '((#f 1) (2 #f)) 1) => '(2 #f)
(define (obtener-fila tablero indice-fila)
  (cond [(or (< indice-fila 0) (null? tablero)) '()]  ; Retorna lista vacía si índice inválido
        [(= indice-fila 0) (car tablero)]  ; Retorna la primera fila
        [else (obtener-fila (cdr tablero) (- indice-fila 1))]))  ; Recursiona

; Contar elementos
; Propósito: Cuenta el número de elementos en una lista.
; Entrada: Una lista.
; Salida: Un entero que representa el número de elementos en la lista.
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
; Propósito: Genera una matriz que representa la distribución de minas en el tablero.
(define (generar-lista-barajada ancho alto num-minas)
  (unless (and (> ancho 0) (> alto 0) (>= num-minas 0))
    (error "Error: ancho, alto deben ser > 0, num-minas >= 0"))
  (let* ([total-casillas (* ancho alto)]
         [lista-plana (barajar (lista1 total-casillas num-minas) total-casillas)])
    (displayln (format "lista-plana: ~a" lista-plana))
    (let ([matriz (list->matriz lista-plana ancho alto)])
      (displayln (format "lista-barajada generada: ~a" matriz))
      matriz)))

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