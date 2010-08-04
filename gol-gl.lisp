(defpackage :gol-gl-frontend
  (:use #:cl #:cl-opengl #:cl-glu #:cl-glut #:gol)
  (:export #:run)
  (:shadow #:get-string #:close #:get #:special))

(in-package :gol-gl-frontend)
(defvar *cells* nil)
(defvar *new-cells nil)
(defvar *chooser-coords* (list 0 0))
(defvar *generation-timer* (get-universal-time))
(defvar *paused* nil)
(defvar *pause-status-coords* nil)

(defun display-cells()
  (do-cells *cells*
            (let ((x gol::x)(y gol::y)(cell gol::cell))
            (with-pushed-matrix 
              (gl:translate x (- y) 0)
              (glut:wire-cube 1)
              (if cell
                  (glut:solid-sphere 0.5 50 50)))))
  (if (and (>= (- (get-universal-time) *generation-timer*) 1) (not *paused*))
    (progn
      (update-generation-timer)
      (gol:next-generation *cells*))))

(defun display-chooser()
  (with-pushed-matrix
    (gl:color 0.5 0 0)
    (gl:translate (first *chooser-coords*) (- (second *chooser-coords*)) 1)
  (glut:solid-torus 0.1 0.3 4 20)))

(defun update-generation-timer()
  (setf *generation-timer* (get-universal-time)))

(defun toggle-pause()
  (if *paused* (update-generation-timer))
  (setf *paused* (not *paused*)))

(defun display-pause-status()
  (with-pushed-matrix
    (let ((coords (multiple-value-list (glu:un-project (first *pause-status-coords*) (second *pause-status-coords*) 0))))
            (gl:color 1.0 0 0)
          (setf (nth 2 coords) 0)
          (apply #'gl:translate coords)
          (glut:solid-cube 1))))

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
  (glu:look-at 0 0 10 0 0 0 0 1 0)
  ;; modeling transformation
  (gl:scale 1 1 1)
  (display-chooser)
  (display-cells)
  #+l(display-pause-status)
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
        (#\Return (setxycell (first *chooser-coords*) (second *chooser-coords*) *cells* t))
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

(defun rb-cube ()
  (glut:display-window (make-instance 'cube-window)))

(defun run ()
  "Run application"
  (let ((glut:*run-main-loop-after-display* nil)(*cells* (make-live-cells '((nil nil nil t)(t t t nil)(t nil nil nil)(nil t nil t)))))
      (rb-cube)
    (glut:main-loop)))
