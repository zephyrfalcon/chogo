;; graphics.scm

(use tk)

(define (init-graphics)
  (start-tk)
  (tk/wm 'title tk "chogo")
  ;; the following brings the Tk window to the top (but it won't stay there
  ;; if you click on other windows)
  ;; also see: http://stackoverflow.com/a/28312609/27426
  (tk/raise tk)
  (tk#eval-wish "wm attributes . -topmost 1")
  (tk#eval-wish "wm attributes . -topmost 0"))

;; for now, there's only one canvas, which can be accessed globally.
(define *canvas* #f)
(define *turtle* #f) ;; one per canvas, for now
(define *canvas-height* #f)
(define *canvas-width* #f)

(define (make-canvas width height bgcolor)
  (unless *canvas* (init-graphics))
  (set! *canvas* (tk 'create-widget 'canvas
                     #:height height #:width width #:bg bgcolor))
  (tk/pack *canvas* #:expand #t #:fill 'both)
  (set! *turtle* (setup-turtle))
  (set! *canvas-height* height)
  (set! *canvas-width* width)
  (turtle-position-set! *turtle* (list (/ width 2) (/ height 2))))

;; XXX should we associate a turtle with each canvas?
;; XXX do we need to deallocate existing canvases?

(define (draw-line canvas p1 p2 color width)
  (canvas 'create 'line (x-of p1) (y-of p1) (x-of p2) (y-of p2)
          #:fill color #:width width))

;; Convert coordinates from Tk's window system to Cartesian coordinates, and
;; vice versa. These depend on *canvas-width* and *canvas-height*.

(define (window-x->cartesian-x x) (- x (/ *canvas-width* 2)))
(define (window-y->cartesian-y y) (- (/ *canvas-height* 2) y))
(define (window->cartesian point)
  (list (window-x->cartesian-x (x-of point))
        (window-y->cartesian-y (y-of point))))

(define (cartesian-x->window-x x) (+ x (/ *canvas-width* 2)))
(define (cartesian-y->window-y y) (- (/ *canvas-height* 2) y))
(define (cartesian->window point)
  (list (cartesian-x->window-x (x-of point))
        (cartesian-y->window-y (y-of point))))

;;; --- Logo commands ---

(define-logo-function (canvas width height)
  (make-canvas width height 'white))

(define-logo-function (penup)
  (turtle-pen-down?-set! *turtle* #f))
(define-logo-function (pendown)
  (turtle-pen-down?-set! *turtle* #t))
(define-logo-function (penwidth size)
  (turtle-width-set! *turtle* size))

;; accepts strings and symbols that name a color or RGB value,
;; e.g. "black", "#800080"
;; TODO: accept (r g b) lists as well
(define-logo-function (pencolor color)
  (turtle-color-set! *turtle* color))

(define-logo-function (forward distance)
  (turtle-forward *turtle* distance))
(define-logo-function (backward distance)
  (turtle-backward *turtle* distance))
(define-logo-function (right degrees)
  (turtle-right *turtle* degrees))
(define-logo-function (left degrees)
  (turtle-left *turtle* degrees))

;; move the turtle to point (x y) (using Cartesian coordinates)
(define-logo-function (setxy x y)
  (let ((wpoint (cartesian->window (list x y))))
    (turtle-go *turtle* wpoint)))

;; KEEP-OPEN: when run in non-interactive mode, has the effect of
;; keeping the Tk window open when the program is done.
(define-logo-function (keep-open)
  (event-loop))
