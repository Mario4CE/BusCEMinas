; Archivo con la interfaz gráfica
#lang racket/gui

; Librerias de gui 
(require racket/gui/base 
         racket/class
         "logica.rkt") ; Archivo con la lógica del juego

(define ancho-tablero 8)
(define alto-tablero 8)
(define nivel-dificultad 'facil) ; 'facil, 'medio, 'dificil
(define tablero '()) ; Va a ser una matriz, donde 0 es vacio y 1 es mina
(define tablero-visual '()) ; Matriz de botones para la interfaz
(define estado-juego 'activo) ; 'activo, 'ganado, 'perdido
(define casillas-descubiertas 0)
(define total-casillas-seguras 0)

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
    (set! tablero (crear-tablero ancho-tablero alto-tablero))
    (set! estado-juego 'activo)
    (set! casillas-descubiertas 0)
    (set! total-casillas-seguras (- (* ancho-tablero alto-tablero) num-minas))
    
    ; Crear la ventana del juego
    (crear-ventana-juego))

; Función para crear la ventana del juego con el tablero visual
(define (crear-ventana-juego)
  (define game-frame (new frame%
                          [label (format "BusCEMinas - ~ax~a - ~a minas" 
                                        ancho-tablero alto-tablero
                                        (calcular-minas ancho-tablero alto-tablero nivel-dificultad))]
                          [width (+ (* ancho-tablero 40) 100)]
                          [height (+ (* alto-tablero 40) 150)]))
  
  ; Panel principal del juego
  (define game-panel (new vertical-panel% [parent game-frame]))
  
  ; Panel de información del juego
  (define info-game-panel (new horizontal-panel% [parent game-panel]))
  (define minas-label (new message% 
                           [parent info-game-panel] 
                           [label (format "Minas: ~a" 
                                         (calcular-minas ancho-tablero alto-tablero nivel-dificultad))]))
  (define estado-label (new message% 
                            [parent info-game-panel] 
                            [label "Estado: Jugando"]))
  
  ; Panel del tablero
  (define board-panel (new vertical-panel% [parent game-panel]))
  
  ; Crear el tablero visual recursivamente
  (set! tablero-visual (crear-tablero-visual board-panel tablero 0))
  
  ; Panel de botones de acción
  (define action-panel (new horizontal-panel% [parent game-panel]))
  (new button% [parent action-panel] 
               [label "Nuevo Juego"] 
               [callback (lambda (button event) (reiniciar-juego game-frame))])
  (new button% [parent action-panel] 
               [label "Configurar"] 
               [callback (lambda (button event) (send game-frame show #f))])
  
  ; Mostrar la ventana del juego
  (send game-frame show #t))

; Función recursiva para crear el tablero visual usando programación funcional
(define (crear-tablero-visual parent tablero fila)
  (cond [(null? tablero) '()] ; Caso base: fin del tablero
        [else 
         (define row-panel (new horizontal-panel% [parent parent]))
         (define fila-botones (crear-fila-botones row-panel (car tablero) fila 0))
         (cons fila-botones 
               (crear-tablero-visual parent (cdr tablero) (+ fila 1)))]))

; Función recursiva para crear una fila de botones
(define (crear-fila-botones parent fila-datos fila columna)
  (cond [(null? fila-datos) '()] ; Caso base: fin de la fila
        [else
         (define boton (new button% 
                           [parent parent]
                           [label ""]
                           [min-width 35]
                           [min-height 35]
                           [callback (lambda (button event) 
                                      (manejar-click-casilla fila columna button event))]))
         (cons boton 
               (crear-fila-botones parent (cdr fila-datos) fila (+ columna 1)))]))

; FUNCIONES MODULARES VACÍAS PARA FUTURAS IMPLEMENTACIONES

; Función para manejar el click en una casilla
(define (manejar-click-casilla fila columna boton evento)
  (cond [(eq? estado-juego 'activo)
         (cond [(eq? (send evento get-event-type) 'button)
                (descubrir-casilla fila columna boton)]
               [(eq? (send evento get-event-type) 'right-down)
                (marcar-bandera fila columna boton)]
               [else #f])]
        [else #f])) ; Juego terminado, no hacer nada

; Función modular vacía para descubrir una casilla
(define (descubrir-casilla fila columna boton)
  ; TODO: Implementar lógica para descubrir casilla
  ; - Verificar si es mina (terminar juego)
  ; - Calcular minas adyacentes
  ; - Si es 0, descubrir casillas vecinas recursivamente
  (send boton set-label "?")
  (send boton enable #f))

; Función modular vacía para marcar/desmarcar bandera
(define (marcar-bandera fila columna boton)
  ; TODO: Implementar lógica para marcar banderas
  ; - Alternar entre bandera y vacío
  ; - Actualizar contador de minas
  (cond [(string=? (send boton get-label) "🚩")
         (send boton set-label "")]
        [else
         (send boton set-label "🚩")]))

; Función modular vacía para calcular minas adyacentes
(define (calcular-minas-adyacentes fila columna)
  ; TODO: Implementar cálculo de minas adyacentes
  ; - Revisar las 8 casillas alrededor
  ; - Contar cuántas son minas
  0)

; Función modular vacía para descubrir casillas vecinas (cuando es 0)
(define (descubrir-vecinas fila columna)
  ; TODO: Implementar descubrimiento recursivo de vecinas
  ; - Si la casilla tiene 0 minas adyacentes
  ; - Descubrir todas las casillas vecinas recursivamente
  #f)

; Función modular vacía para verificar victoria
(define (verificar-victoria)
  ; TODO: Implementar verificación de victoria
  ; - Contar casillas descubiertas
  ; - Comparar con total de casillas seguras
  (cond [(>= casillas-descubiertas total-casillas-seguras)
         (set! estado-juego 'ganado)
         (mostrar-mensaje-victoria)]
        [else #f]))

; Función modular vacía para mostrar mensaje de victoria
(define (mostrar-mensaje-victoria)
  ; TODO: Implementar mensaje de victoria
  (message-box "¡Felicitaciones!" "¡Has ganado el juego!"))

; Función modular vacía para mostrar mensaje de derrota
(define (mostrar-mensaje-derrota)
  ; TODO: Implementar mensaje de derrota
  ; - Revelar todas las minas
  ; - Mostrar mensaje de game over
  (message-box "¡Oh no!" "¡Has perdido! Encontraste una mina."))

; Función para reiniciar el juego
(define (reiniciar-juego ventana-actual)
  (send ventana-actual show #f)
  (generar-tablero))



; Mostrar ventana
(send frame show #t)