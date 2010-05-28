;; turtle.scm

(define-record turtle
  position
  direction
  pen-down?
  color
  width
  )

(define (setup-turtle)
  "Set up a default turtle."
  (make-turtle '(0 0) 0 #t '(0 0 0) 1))

(define math:pi (atan 0 -1))
(define deg (/ math:pi 180))

;;; --- turtle actions ---

(define (turtle-right turtle degrees)
  (turtle-direction-set! turtle
                         (modulo (+ (turtle-direction turtle) degrees) 360)))

(define (turtle-left turtle degrees)
  (turtle-right turtle (- degrees)))

(define (turtle-forward turtle distance)
  (let* ((p1 (turtle-position turtle))
         (angle (turtle-direction turtle))
         (p2 (list (+ (x-of p1) (* (sin (* angle deg)) distance))
                   (- (y-of p1) (* (cos (* angle deg)) distance)))))
    (when (turtle-pen-down? turtle)
      (draw-line *canvas* p1 p2
                 (turtle-color *turtle*) (turtle-width *turtle*)))
    (turtle-position-set! turtle p2)))

(define (turtle-backward turtle distance)
  (turtle-forward turtle (- distance)))

;; TODO: turtle-go : go to a given point directly
;; TODO: turtle-toward : turn toward the given point
;; TODO: turtle-distance : distance between turtle and the given point
