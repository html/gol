(defpackage :gol-gl-frontend
  (:use #:cl #:cl-opengl #:cl-glu #:cl-glut)
  (:export #:run)
  (:shadow #:get-string #:close #:get #:special))

(in-package :gol-gl-frontend)

(defun display-cells()
  (let ((cells '((t nil nil)(nil nil nil))))
    (dotimes (y (length cells))
      (dotimes (x (length (nth y cells)))
                (with-pushed-matrix 
                  (let ((value (nth y (nth x cells))))
                  (gl:translate x (- y) 0)
                  (if value
                      (glut:solid-cube 1)
                      (glut:wire-cube 1))))))))

;;;;;;;;;;;;;;;;;;; OPENGL DEFS ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cube-window (glut:window)
  ()
  (:default-initargs :width 500 :height 500 :title "cube.lisp"
                     :mode '(:single :rgb)))

(defmethod glut:display-window :before ((w cube-window))
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :flat))

(defmethod glut:display ((w cube-window))
  (gl:clear :color-buffer)
  (gl:color 1 1 1)
  (gl:load-identity) ; clear the matrix
  ;; viewing transformation
  (glu:look-at 0 0 10 0 0 0 0 1 0)
  ;; modeling transformation
  (gl:scale 1 1 1)
  (glut:wire-cube 1)
  (display-cells)
  (gl:flush))

(defmethod glut:reshape ((w cube-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:frustum -1 1 -1 1 1.5 20)
  (gl:matrix-mode :modelview))

(defmethod glut:keyboard ((w cube-window) key x y)
  (declare (ignore x y))
  (when (eql key #\Esc)
    (glut:destroy-current-window)))

(defun rb-cube ()
  (glut:display-window (make-instance 'cube-window)))

(defun run ()
  "Run application"
  (let ((glut:*run-main-loop-after-display* nil))
      (rb-cube)
    (glut:main-loop)))
