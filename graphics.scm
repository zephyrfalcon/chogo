;; graphics.scm

(use tk)

(define (init-graphics)
  (start-tk))

;; for now, there's only one canvas, which can be accessed globally.
(define *canvas* #f)

(define (make-canvas width height bgcolor)
  (unless *canvas* (init-graphics))
  (set! *canvas* (tk 'create-widget 'canvas
                     #:height height #:width width #:bg bgcolor))
  (tk/pack *canvas* #:expand #t #:fill 'both))


