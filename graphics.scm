;; graphics.scm

(use tk)

(define (init-graphics)
  (start-tk))

;; for now, there's only one canvas, which can be accessed globally.
(define *canvas* #f)
(define *turtle* #f) ;; one per canvas, for now

(define (make-canvas width height bgcolor)
  (unless *canvas* (init-graphics))
  (set! *canvas* (tk 'create-widget 'canvas
                     #:height height #:width width #:bg bgcolor))
  (tk/pack *canvas* #:expand #t #:fill 'both)
  (set! *turtle* (setup-turtle))
  (turtle-position-set! *turtle* (list (/ width 2) (/ height 2))))


;; XXX should we associate a turtle with each canvas?
;; XXX do we need to deallocate existing canvases?

;; TODO: color, thickness of line, maybe kind of line, etc
(define (draw-line canvas p1 p2)
  (canvas 'create 'line (x-of p1) (y-of p1) (x-of p2) (y-of p2)
          #:fill 'black))

;;; --- Logo commands ---

(define-logo-function (penup)
  (turtle-pen-down?-set! #f))
(define-logo-function (pendown)
  (turtle-pen-down?-set! #t))

(define-logo-function (forward distance)
  (turtle-forward *turtle* distance))
(define-logo-function (backward distance)
  (turtle-backward *turtle* distance))
(define-logo-function (right degrees)
  (turtle-right *turtle* degrees))
(define-logo-function (left degrees)
  (turtle-left *turtle* degrees))

;; temporary... I think...
(define-logo-function (keep-open)
  (event-loop))
