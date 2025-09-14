#lang racket/gui

; Librerias de gui 
(require racket/gui/base 
         racket/class
         "logica.rkt") ; Archivo con la lógica del juego

(define ancho-tablero 8)
(define alto-tablero 8)
(define nivel-dificultad 'facil) ; 'facil, 'medio, 'dificil


; Función para validar dimensiones del tablero 
; Estas dimensiones máximas y mínimas son especificadas 
; en el documento
(define (validar-dimension dim)
    (and (integer? dim) (>= dim 8) (<= dim 15)))

; Esta parte es para actualizar la info una vez el usuario seleccione
(define (actualizar-info)
    (define ancho-texto (send ancho-field get-value))
    (define alto-texto (send alto-field get-value))
    (define ancho-num (string->number ancho-texto))
    (define alto-num (string->number alto-texto))
    
    ; Valida las dimensiones (8 <= n <= 15)
    (if (and ancho-num alto-num
             (validar-dimension ancho-num)
             (validar-dimension alto-num))
        
        ; Obtiene los valores de dificultad seleccionados
        (let* ([diff-index (send dificultad-choice get-selection)]
               [diff-nivel (case diff-index 
                            [(0) 'facil]
                            [(1) 'medio]
                            [(2) 'dificil])]
               [num-minas (calcular-minas ancho-num alto-num diff-nivel)])
              (set! ancho-tablero ancho-num)
              (set! alto-tablero alto-num)
              (set! nivel-dificultad diff-nivel)
              ; Pone los datos actualizados en el label de informacion
              (send info-label set-label 
                    (format "Tablero: ~ax~a => Minas: ~a"
                            ancho-num alto-num num-minas)))
            (send info-label set-label "Dimensiones inválidas (8-15)")))

; Crear la ventana principal 
(define frame (new frame% 
                    [label "Configuración de BusCEMinas"]
                    [width 450]
                    [height 300]))

; Panel principal
(define main-panel (new vertical-panel% [parent frame]))

; Panel para dimensiones
(define dim-panel (new horizontal-panel% [parent main-panel]))


; Etiquetas y campos de entrada (con callbacks incluidos)
(new message% [parent dim-panel] [label "Ancho (8-15)"])
(define ancho-field (new text-field% 
                         [parent dim-panel]
                         [label ""]
                         [init-value "8"]
                         [callback (lambda (field event) (actualizar-info))]))

(new message% [parent dim-panel] [label "Alto (8-15)"])
(define alto-field (new text-field%
                        [parent dim-panel]
                        [label ""]
                        [init-value "8"]
                        [callback (lambda (field event) (actualizar-info))]))


;Panel para dificultad 
(define diff-panel (new vertical-panel% [parent main-panel]))
(new message% [parent diff-panel] [label "Nivel de dificultad"])

;selección de dificultad (choice)
(define dificultad-choice (new radio-box%
                               [label ""]
                               [choices '("Fácil (10%)" "Medio (15%)" "Dificil (20%)")]
                               [parent diff-panel]
                               [selection 0]
                               [callback (lambda (choice event) (actualizar-info))]))


; Panel para info
(define info-panel (new vertical-panel% [parent main-panel]))
(define info-label (new message%
                        [parent info-panel]
                        [label "Tablero: 8x8 => minas: 7"]
                        [min-width 300])) ;datos iniciales


; botón para generar tablero
(define generar-btn (new button%
                         [parent main-panel]
                         [label "Generar Tablero"]
                         [callback (lambda (button event)
                                    (generar-tablero))]))

; funcion que genera el tablero 
(define (generar-tablero)
    (define num-minas (calcular-minas ancho-tablero alto-tablero nivel-dificultad))
    
    (message-box "Tablero Generado"
                 (format "Tablero ~ax~a creado con ~a minas. Nivel: ~a"
                         ancho-tablero alto-tablero num-minas nivel-dificultad)))

; Mostrar ventana
(send frame show #t)