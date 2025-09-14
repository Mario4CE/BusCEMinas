#lang racket

; Exportar las funciones que quieres usar en otros archivos
(provide calcular-minas)

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

