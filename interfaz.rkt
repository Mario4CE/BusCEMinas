#lang racket/gui
(require racket/class
         "logica.rkt"
         "adyacentes_grafica.rkt"
         "adyacentes.rkt")

(define ancho-tablero 8)
(define alto-tablero 8)
(define nivel-dificultad 'facil)
(define tablero '()) ; Matriz para tablero-logico
(define tablero-visual '()) ; Matriz de botones para la interfaz
(define estado-juego 'activo) ; 'activo, 'ganado, 'perdido
(define casillas-descubiertas-ref (box 0)) ; Box to track discovered cells
(define total-casillas-seguras 0)
(define lista-barajada '()) ; Matriz con valores 0 y 1

(define (es-mina x y)
  (cond ((eq? (list-ref (list-ref lista-barajada x) y) 1) #t) (else #f)))

; Función para validar dimensiones del tablero
(define (validar-dimension dim)
  (and (integer? dim) (>= dim 8) (<= dim 15)))

; Función para actualizar la información del tablero
(define (actualizar-info)
  (define ancho-texto (send ancho-field get-value))
  (define alto-texto (send alto-field get-value))
  (define ancho-num (string->number ancho-texto))
  (define alto-num (string->number alto-texto))
  (if (and ancho-num alto-num
           (validar-dimension ancho-num)
           (validar-dimension alto-num))
      (let* ([diff-index (send dificultad-choice get-selection)]
             [diff-nivel (case diff-index
                           [(0) 'facil]
                           [(1) 'medio]
                           [(2) 'dificil])]
             [num-minas (calcular-minas ancho-num alto-num diff-nivel)])
        (set! ancho-tablero ancho-num)
        (set! alto-tablero alto-num)
        (set! nivel-dificultad diff-nivel)
        (send info-label set-label
              (format "Tablero: ~ax~a => Minas: ~a" ancho-num alto-num num-minas)))
      (send info-label set-label "Dimensiones inválidas (8-15)")))

; Crear la ventana principal
(define frame (new frame%
                   [label "Configuración de BusCEMinas"]
                   [width 450]
                   [height 300]))

(define main-panel (new vertical-panel% [parent frame]))
(define dim-panel (new horizontal-panel% [parent main-panel]))

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

(define diff-panel (new vertical-panel% [parent main-panel]))
(new message% [parent diff-panel] [label "Nivel de dificultad"])
(define dificultad-choice (new radio-box%
                               [label ""]
                               [choices '("Fácil (10%)" "Medio (15%)" "Difícil (20%)")]
                               [parent diff-panel]
                               [selection 0]
                               [callback (lambda (choice event) (actualizar-info))]))

(define info-panel (new vertical-panel% [parent main-panel]))
(define info-label (new message%
                        [parent info-panel]
                        [label "Tablero: 8x8 => minas: 7"]
                        [min-width 300]))

(define generar-btn (new button%
                         [parent main-panel]
                         [label "Generar Tablero"]
                         [callback (lambda (button event)
                                     (generar-tablero))]))

; Función para generar el tablero
(define (generar-tablero)
  (define num-minas (calcular-minas ancho-tablero alto-tablero nivel-dificultad))
  (set! lista-barajada (generar-lista-barajada ancho-tablero alto-tablero num-minas))
  (displayln (format "lista-barajada: ~a" lista-barajada))
  (set! tablero (crear-tablero ancho-tablero alto-tablero))
  (set! estado-juego 'activo)
  (set-box! casillas-descubiertas-ref 0)
  (set! total-casillas-seguras (- (* ancho-tablero alto-tablero) num-minas))
  (crear-ventana-juego))

(define modo-bandera-activo #f)
(define contador-banderas-correctas 0)

; Función para crear la ventana del juego
(define (crear-ventana-juego)
  (define game-frame (new frame%
                          [label (format "BusCEMinas - ~ax~a - ~a minas"
                                         ancho-tablero alto-tablero
                                         (calcular-minas ancho-tablero alto-tablero nivel-dificultad))]
                          [width (+ (* ancho-tablero 40) 100)]
                          [height (+ (* alto-tablero 40) 150)]))
  (define game-panel (new vertical-panel% [parent game-frame]))
  (define info-game-panel (new horizontal-panel% [parent game-panel]))
  (define minas-label (new message%
                           [parent info-game-panel]
                           [label (format "Minas: ~a"
                                          (calcular-minas ancho-tablero alto-tablero nivel-dificultad))]))
  (define estado-label (new message%
                            [parent info-game-panel]
                            [label "Estado: Jugando"]))
  (define board-panel (new vertical-panel% [parent game-panel]))
  (set! tablero-visual (crear-tablero-visual board-panel tablero 0))
  (define action-panel (new horizontal-panel% [parent game-panel]))
  (new button% [parent action-panel]
       [label "Nuevo Juego"]
       [callback (lambda (button event) (reiniciar-juego game-frame))])
  (new button% [parent action-panel]
       [label "Configurar"]
       [callback (lambda (button event) (send game-frame show #f))])
  (define bandera-btn (new button% [parent action-panel]
                           [label "Modo: Normal"]
                           [callback (lambda (button event) 
                                       (set! modo-bandera-activo (not modo-bandera-activo))
                                       (send button set-label 
                                             (if modo-bandera-activo 
                                                 "Modo: Bandera" 
                                                 "Modo: Normal")))]))
  (send game-frame show #t))


; Crear tablero visual
(define (crear-tablero-visual parent tablero fila)
  (cond [(null? tablero) '()]
        [else
         (define row-panel (new horizontal-panel% [parent parent]))
         (define fila-botones (crear-fila-botones row-panel (car tablero) fila 0))
         (cons fila-botones
               (crear-tablero-visual parent (cdr tablero) (+ fila 1)))]))
               
; Crear fila de botones
(define (crear-fila-botones parent fila-datos fila columna)
  (cond
    [(null? fila-datos) '()]
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

; Manejar clic en una casilla
(define (manejar-click-casilla fila columna boton evento)
  (displayln (format "Manejar click en: (~a, ~a), evento ~a" fila columna evento))
  (cond
    [(eq? estado-juego 'activo)
     (cond
       [(eq? (send evento get-event-type) 'button)
        (cond
          [modo-bandera-activo 
           (marcar-bandera fila columna boton)]
          [else
           (with-handlers 
               ([exn:fail?
                 (lambda (exn)
                   (displayln (format "Excepción en (~a,~a): ~a" fila columna (exn-message exn)))
                   (if (string=? (exn-message exn) "¡Mina encontrada!")
                       (begin
                         (set! estado-juego 'perdido)
                         (mostrar-mensaje-derrota))
                       (displayln "Error no relacionado con mina, ignorando")))])
             (descubrir-casilla fila columna boton))])])]
    [else #f]))

; Marcar/desmarcar bandera
(define (marcar-bandera fila columna boton)
  (define num-minas (calcular-minas ancho-tablero alto-tablero nivel-dificultad))
  (define minado (es-mina fila columna))
  (cond [(string=? (send boton get-label) "?")
         (send boton set-label "")
         (when minado
           (set! contador-banderas-correctas (- contador-banderas-correctas 1)))]
        [else
         (send boton set-label "?")
         (when minado
           (set! contador-banderas-correctas (+ contador-banderas-correctas 1)))])

  (displayln (format "Es mina: ~a, contador: ~a" minado contador-banderas-correctas))
  (verificar-victoria))

; Descubrir una casilla
(define (descubrir-casilla fila columna boton)
  (when (and (eq? estado-juego 'activo) (send boton is-enabled?))
    (displayln (format "Clic en (~a,~a), botón habilitado: ~a" fila columna (send boton is-enabled?)))
    (define valor-casilla (obtener-valor-casilla lista-barajada fila columna))
    (displayln (format "Casilla (~a,~a): valor=~a" fila columna valor-casilla))
    (when (and valor-casilla (= valor-casilla 1))
      (displayln (format "Mina encontrada en (~a,~a)" fila columna))
      (send boton set-label "MINA")
      (set! estado-juego 'perdido)
      (mostrar-mensaje-derrota)
      (error "¡Mina encontrada!"))

    (define minas (calcular-minas-adyacentes lista-barajada fila columna))
    (displayln (format "Casilla (~a,~a): minas adyacentes=~a" fila columna minas))

    (cond
      [(> minas 0)
       (displayln (format "Marcando (~a,~a) con número: ~a" fila columna minas))
       (send boton set-label (number->string minas))
       (send boton enable #f)
       (set-box! casillas-descubiertas-ref (add1 (unbox casillas-descubiertas-ref)))
       (set! tablero (actualizar-casilla tablero fila columna minas))
       ]
      [else
       (displayln (format "Click en (~a,~a) era 0 → marcando y expandiendo vecinos..." fila columna))
       (send boton set-label "")
       (send boton enable #f)
       (set-box! casillas-descubiertas-ref (add1 (unbox casillas-descubiertas-ref)))
       (displayln (format "Tablero lógico antes de expansión: ~a" tablero))
       (set! tablero
             (descubrir-vecinas-gui fila columna
                                    tablero-visual tablero
                                    ancho-tablero alto-tablero
                                    lista-barajada casillas-descubiertas-ref))
       (displayln (format "Tablero lógico tras expansión: ~a" tablero))
       ])))



; Verificar victoria
(define (verificar-victoria)
  (define num-minas (calcular-minas ancho-tablero alto-tablero nivel-dificultad))
  (cond  [(= num-minas contador-banderas-correctas)
         (set! estado-juego 'ganado)
         (mostrar-mensaje-victoria)]
        [else #f]))

; Mostrar mensaje de victoria
(define (mostrar-mensaje-victoria)
  (message-box "¡Felicitaciones!" "¡Has ganado el juego!"))

; Mostrar mensaje de derrota
(define (mostrar-mensaje-derrota)
  (for ([i (in-range alto-tablero)]
        [j (in-range ancho-tablero)])
    (when (= (obtener-valor-casilla lista-barajada i j) 1)
      (define boton (list-ref (list-ref tablero-visual i) j))
      (send boton set-label "MINA")
      (send boton enable #f)))
  (message-box "¡Oh no!" "¡Has perdido! Encontraste una mina."))

; Reiniciar el juego
(define (reiniciar-juego ventana-actual)
  (set! contador-banderas-correctas 0)
  (set! modo-bandera-activo #f)
  (send ventana-actual show #f)
  (generar-tablero))

(provide frame) ; Exportar frame para que main.rkt pueda usarlo