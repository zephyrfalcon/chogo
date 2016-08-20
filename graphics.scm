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
(define (draw-line canvas p1 p2 color width)
  (canvas 'create 'line (x-of p1) (y-of p1) (x-of p2) (y-of p2)
          #:fill color #:width width))

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

;; KEEP-OPEN: when run in non-interactive mode, has the effect of
;; keeping the Tk window open when the program is done.
(define-logo-function (keep-open)
  (event-loop))
