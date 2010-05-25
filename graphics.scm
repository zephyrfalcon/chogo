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

;; XXX should we associate a turtle with each canvas?
;; XXX turtle should go to middle of canvas by default
;; XXX do we need to deallocate existing canvases?

