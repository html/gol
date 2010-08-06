(defpackage :gol-gl-frontend
  (:use #:cl #:cl-opengl #:cl-glu #:cl-glut #:gol)
  (:export #:run)
  (:shadow #:get-string #:close #:get #:special))

(in-package :gol-gl-frontend)
(defparameter *cells* nil)
(defparameter *new-cells nil)
(defparameter *chooser-coords* (list 0 0))
(defparameter *generation-timer* (get-universal-time))
(defparameter *paused* nil)
(defparameter *pause-status-coords* nil)
(defparameter *extreme-points* nil)
(defparameter *extreme-points-grow* nil)
(defparameter *display-borders-counter* 0)

(defun display-cells()
  (gl:line-width 1)
  (gl:material :front :ambient-and-diffuse #(1.0 1.0 1.0 1.0)) ; red

  (do-cells *cells*
            (let ((x gol::x)(y gol::y)(cell gol::cell))
            (with-pushed-matrix 
              (gl:translate x (- y) 0)
              (glut:wire-cube 1)
              (if cell
                  (glut:solid-sphere 0.5 50 50)))))

  (if (and (>= (- (get-universal-time) *generation-timer*) 1) (not *paused*))
    (progn

      (dolist (way (list :up :down :left :right))
        (let ((old-coord (getf *extreme-points* way))
              (new-coord (gol:extreme-coord *cells* way)))
             (setf (getf *extreme-points-grow* way) (if old-coord (- new-coord old-coord) 0))
             (setf (getf *extreme-points* way) new-coord)))

      (update-generation-timer)
      (gol:next-generation *cells*))))

(defun display-chooser()
  (with-pushed-matrix
    #+l(gl:color 0.5 0 0)
    (gl:translate (first *chooser-coords*) (- (second *chooser-coords*)) 1)
  (glut:solid-torus 0.1 0.3 4 20)))

(defun display-borders()
  (with-pushed-matrix
    (gl:line-width 10)
    (gl:material :front :ambient-and-diffuse #(1 .5 0 1.0)) ; red
    (gl:with-primitives :line-strip
                        (incf *display-borders-counter*)
                        (unless (null *extreme-points*)
                        (let* ((lambda (lambda (way fun)(funcall fun (gol:extreme-coord *cells* way) (+ (getf *extreme-points* way) (* *display-borders-counter* (/ (getf *extreme-points-grow* way) 200))))))
                              (lambda2 (lambda (way)(min (gol:extreme-coord *cells* way) (+ (getf *extreme-points* way) (* *display-borders-counter* (/ (getf *extreme-points-grow* way) 200))))))
                              (up (- (funcall lambda :up #'max) 0.5))
                              (down (- (funcall lambda :down #'min) 0.5))
                              (left (- (- (funcall lambda :left #'max) 0.5)))
                              (right (- (- (funcall lambda :right #'min) 0.5))))
                          (dolist (i (list (list up left) (list up right) (list down right) (list down left) (list up left)))
                            (gl:vertex (first i) (second i) .5)))))))

(defun update-generation-timer()
  (setf *display-borders-counter* 0)
  (setf *generation-timer* (get-universal-time)))

(defun toggle-pause()
  (if *paused* (update-generation-timer))
  (setf *paused* (not *paused*)))

(defun display-pause-status()
  (with-pushed-matrix
    (let ((coords (multiple-value-list (glu:un-project (- (first *pause-status-coords*) 50) (- (second *pause-status-coords*) 50) (aref (gl:read-pixels 100 100 1 1 :depth-component :float) 0)))))
            (gl:color 1.0 0 0)
          #+l(setf (nth 2 coords) 0)
          (apply #'gl:translate coords)
          (if *paused* (glut:solid-cube 5) (glut:solid-sphere 5 50 50))
          )))

(defun move-chooser(way)
  (case way
    (:down (incf (second *chooser-coords*)))
    (:up (decf (second *chooser-coords*)))
    (:right (incf (first *chooser-coords*)))
    (:left (decf (first *chooser-coords*)))))

;;;;;;;;;;;;;;;;;;; OPENGL DEFS ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cube-window (glut:window)
  ()
  (:default-initargs :width 500 :height 500 :title "cube.lisp"
                     :mode '(:double :rgb :depth)))

(defmethod glut:display-window :before ((w cube-window))
  (gl:light :light0 :position #(5.0 5.0 10.0 0.0))
  (gl:enable :cull-face :lighting :light0 :depth-test)
  (gl:clear-color 0 0 0 0)
  ;;;(gl:shade-model :flat)
  )

(defmethod glut:display ((w cube-window))
  (gl:clear :color-buffer :depth-buffer)
  (gl:load-identity) ; clear the matrix
  ;; viewing transformation
  (glu:look-at 0 0 (* (max (x-size *cells*) (y-size *cells*)) 2) 0 0 0 0 2 0)
  ;; modeling transformation
  (gl:scale 1 1 1)
  (display-chooser)
  (display-cells)
  (display-borders)
  (display-pause-status)
  (gl:flush)
  (glut:swap-buffers))
  
(defmethod glut:idle ((window cube-window))
  (glut:post-redisplay))

(defmethod glut:reshape ((w cube-window) width height)
  (setf *pause-status-coords* (list width height))
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)    ; select the projection matrix
  (gl:load-identity)              ; reset the matrix
  (glu:perspective 45 (/ width (max height 1)) 1/10 100)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (glut:post-redisplay))

(defun setxycell-by-chooser-coords()
  (setxycell (first *chooser-coords*) (second *chooser-coords*) *cells* t))

(defmethod glut:keyboard ((w cube-window) key x y)
  (declare (ignore x y))
  (move-chooser (case key 
                  (#\w :up)
                  (#\s :down)
                  (#\d :right)
                  (#\a :left)))
  (if (equal key #\Esc)
    (glut:destroy-current-window)
    (progn
      (case key
        (#\Space (toggle-pause))
        (#\Return (setxycell-by-chooser-coords))
        (#\Esc )
        (t (print key)))
      (glut:post-redisplay))))

(defmethod glut:special ((window cube-window) special-key x y)
  (declare (ignore x y))
    (move-chooser (case special-key
      (:key-up :up)
      (:key-down :down)
      (:key-left :left)
      (:key-right :right)))
    (glut:post-redisplay))

#+l(defmethod glut:mouse ((w cube-window) button state x y)
  (format t "~A~%" (list w button state x y)))

(defun run-game-of-life()
  (glut:display-window (make-instance 'cube-window)))

(defun run ()
  "Run application"
  (let ((glut:*run-main-loop-after-display* nil)(*cells* (make-live-cells '((nil nil nil t)(t t t nil)(t nil nil nil)(nil t nil t)))))
      (run-game-of-life)
    (glut:main-loop)))
