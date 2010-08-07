(defpackage :gol-gl-frontend
  (:use #:cl #:cl-opengl #:cl-glu #:cl-glut #:gol)
  (:export #:run)
  (:shadow #:get-string #:close #:get #:special))

(in-package :gol-gl-frontend)
(defparameter *cells* nil)
(defparameter *new-cells nil)
(defparameter *chooser-coords* (list 0 0))
(defparameter *generation-timer* (get-internal-real-time))
(defparameter *paused* nil)
(defparameter *window-size* nil)
(defparameter *extreme-points* nil)
(defparameter *extreme-points-grow* nil)
(defparameter *display-borders-counter* 0)
(defparameter *generation-timer-timeout* 1)
(defparameter *fix-coords* t)

(defun center-view()
  (gl:matrix-mode :projection)    ; select the projection matrix
  (gl:load-identity)              ; reset the matrix
  ;;(glu:perspective 45 (/ width (max height 1)) 1/10 100)
  (let* ((diam (* (apply #'max (mapcar #'abs 
                      (list 
                        (extreme-coord *cells* :up) 
                        (extreme-coord *cells* :down) 
                        (extreme-coord *cells* :left) 
                        (extreme-coord *cells* :right)))) 1))
         (aspect (apply #'/ *window-size*))
         (left (- diam))
         (right diam) 
         (top (- diam))
         (bottom diam))
         (if (< aspect 1)
           (progn
             (setf bottom (/ bottom aspect))
             (setf top    (/ top aspect)))
           (progn
             (setf left (* left aspect))
             (setf right (* right aspect))))
         (gl:ortho left right top bottom 1 1000))
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun set-glider()
  (setf (slot-value *cells* 'cells-matrix)
        '((nil t nil)
         (nil nil t)
         (t   t   t)))
  (setf (slot-value *cells* 'cells-x-out) 0)
  (setf (slot-value *cells* 'cells-y-out) 0)
  (setf *chooser-coords* (list 0 0)))

(defun update-generation()
  (let ((old-extreme-points nil))

      (dolist (way (list :up :down :left :right))
        (let ((old-coord (getf *extreme-points* way))
              (new-coord (gol:extreme-coord *cells* way)))
             (setf (getf old-extreme-points way) new-coord)
             (setf (getf *extreme-points* way) new-coord)))

     (update-generation-timer)
     (gol:next-generation *cells*)

     (dolist (way (list :up :down :left :right))
        (let ((old-coord (getf old-extreme-points way)))
        (setf (getf *extreme-points-grow* way) 
                           (if (or (null old-coord) (= old-coord (gol:extreme-coord *cells* way))) 
                             0 
                             (if (> old-coord (gol:extreme-coord *cells* way)) -1 1))))))
  (center-view))

(defun display-cells()
  (gl:line-width 1)
  (do-cells *cells*
            (let ((x gol::x)(y gol::y)(cell gol::cell))
            (with-pushed-matrix 
              (gl:translate x (- y) 0)
              (gl:color 0.2 0.2 0.2)
              (glut:wire-cube 1)
              (gl:color 1.0 1.0 1.0)
              (if cell
                  (glut:solid-sphere 0.5 50 50)))))

  (if (and (>= (- (get-internal-real-time) *generation-timer*) (* *generation-timer-timeout* internal-time-units-per-second)) (not *paused*))
    (update-generation)))

(defun display-chooser()
  (gl:color 0.0 1.0 0.0)
  (with-pushed-matrix
    (gl:translate (first *chooser-coords*) (- (second *chooser-coords*)) 1)
  (glut:solid-torus 0.1 0.3 4 20)))

(defun display-borders()
  (gl:color 1.0 1.0 0.0)
  (with-pushed-matrix
    (gl:line-width 2)
    (gl:with-primitives :line-strip
                        (incf *display-borders-counter*)
                        (unless (null *extreme-points*)
                        (let* ((lambda (lambda (way fun)(funcall fun (gol:extreme-coord *cells* way) 
                                                                 (+ 
                                                                   (getf *extreme-points* way) 
                                                                   (* *display-borders-counter* (/ (getf *extreme-points-grow* way) 100))
                                                                   ))))
                              (up (+ (- (funcall lambda :up #'max)) 0.5))
                              (down (+ (- (funcall lambda :down #'min)) 0.5))
                              (left (- (funcall lambda :left #'max) 0.5))
                              (right (- (funcall lambda :right #'min) 0.5)))
                          (dolist (i (list (list up left) (list up right) (list down right) (list down left) (list up left)))
                            (gl:vertex  (second i) (first i) 1)))))))

(defun update-generation-timer()
  (setf *display-borders-counter* 0)
  (setf *generation-timer* (get-internal-real-time)))

(defun toggle-pause()
  (if *paused* (update-generation-timer))
  (setf *paused* (not *paused*)))

(defun pixel->coords(left top)
  (multiple-value-list (glu:un-project left top (aref (gl:read-pixels left top 1 1 :depth-component :float) 0))))

(defun display-pause-status()
    (let* ((width (first *window-size*))
           (height (second *window-size*))
           (radius (- (first (pixel->coords 50 0)) (first (pixel->coords 0 0)))))
           (gl:color 1.0 1.0 .0)
           (gl:rotate 90 0 1 0)
           (with-pushed-matrix
           (let ((width (- width (/ width 10)))
                (height (- height (/ height 10))))
           (if (not *paused*)
            (progn
              (apply #'gl:translate (pixel->coords width height))
              (glut:solid-cube radius))
            (progn 
              (apply #'gl:translate (pixel->coords (- width 25) height))
              (glut:solid-cone (/ radius 2) radius 10 50)))))))

(defun add-row(way)
  (let ((row (lambda() (list (make-list (length (first (slot-value *cells* 'cells-matrix))))))))
  (case way
    (:up (setf (slot-value *cells* 'cells-matrix) (append (funcall row) (slot-value *cells* 'cells-matrix)))
         (incf (slot-value *cells* 'cells-y-out)))
    (:down (setf (slot-value *cells* 'cells-matrix) (append (slot-value *cells* 'cells-matrix) (funcall row))))
    (:left (setf (slot-value *cells* 'cells-matrix) (mapcar (lambda (item) (append (list nil) item)) (slot-value *cells* 'cells-matrix)))
         (incf (slot-value *cells* 'cells-x-out)))
    (:right (setf (slot-value *cells* 'cells-matrix) (mapcar (lambda (item) (append item (list nil))) (slot-value *cells* 'cells-matrix)))))))

(defun remove-row(way)
  (case way
    (:up 
      (when (> (y-size *cells*) 1)
        (pop (slot-value *cells* 'cells-matrix))
        (decf (slot-value *cells* 'cells-y-out))
        (if (chooser-out-of-bounds-p) (incf (second *chooser-coords*)))))
    (:down 
      (when (> (y-size *cells*) 1)
          (setf (slot-value *cells* 'cells-matrix) (butlast (slot-value *cells* 'cells-matrix)))
          (if (chooser-out-of-bounds-p) (decf (second *chooser-coords*)))))
    (:left
      (when (> (x-size *cells*) 1)
        (setf (slot-value *cells* 'cells-matrix) (mapcar #'cdr (slot-value *cells* 'cells-matrix)))
        (if (chooser-out-of-bounds-p) (incf (first *chooser-coords*)))))
    (:right
      (when (> (x-size *cells*) 1)
        (setf (slot-value *cells* 'cells-matrix) (mapcar #'butlast (slot-value *cells* 'cells-matrix)))
        (if (chooser-out-of-bounds-p) (decf (first *chooser-coords*)))))))

(defun chooser-out-of-bounds-p()
  (or
    (< (second *chooser-coords*) (extreme-coord *cells* :up))
    (> (second *chooser-coords*) (extreme-coord *cells* :down))
    (< (first  *chooser-coords*) (extreme-coord *cells* :left))
    (> (first  *chooser-coords*) (extreme-coord *cells* :right))))

(defun move-chooser(way)
  (case way
    (:down  (if (>= (incf (second *chooser-coords*)) (extreme-coord *cells* :down))  (add-row :down)))
    (:up    (if (<  (decf (second *chooser-coords*)) (extreme-coord *cells* :up))    (add-row :up)))
    (:right (if (>= (incf (first *chooser-coords*))  (extreme-coord *cells* :right)) (add-row :right)))
    (:left  (if (<  (decf (first *chooser-coords*))  (extreme-coord *cells* :left))  (add-row :left)))))



;;;;;;;;;;;;;;;;;;; OPENGL DEFS ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cube-window (glut:window)
  ()
  (:default-initargs :width 500 :height 500 :title "cube.lisp"
                     :mode '(:double :rgb :depth)))

(defmethod glut:display-window :before ((w cube-window))
  (gl:light :light0 :position #(5.0 5.0 10.0 0.0))
  (gl:enable :lighting :light0 :depth-test)
  (gl:enable :lighting)
  (gl:color-material :front-and-back :ambient-and-diffuse)
  (gl:enable :color-material)
  (gl:clear-color 0 0 0 0))

(defmethod glut:display ((w cube-window))
  (gl:clear :color-buffer :depth-buffer)
  (gl:load-identity) ; clear the matrix
  ;; viewing transformation
  (glu:look-at 0 0 (* (max (x-size *cells*) (y-size *cells*)) 2) 0 0 0 0 2 0)
  ;; modeling transformation
  (gl:scale 1 1 1)
  (if *fix-coords*
      (gl:translate (- (- (/ (x-size *cells*) 2) (slot-value *cells* 'cells-x-out))) (- (/ (y-size *cells*) 2) (slot-value *cells* 'cells-y-out)) 0))
  (display-chooser)
  (display-cells)
  (display-borders)
  (display-pause-status)
  (gl:flush)
  (glut:swap-buffers))
  
(defmethod glut:idle ((window cube-window))
  (glut:post-redisplay))

(defmethod glut:reshape ((w cube-window) width height)
  (setf *window-size* (list width height))
  (gl:viewport 0 0 width height)
  (center-view)
  (glut:post-redisplay))

(defun setxycell-by-chooser-coords()
  (setxycell (first *chooser-coords*) (second *chooser-coords*) *cells* (not (gol:xycell (first *chooser-coords*) (second *chooser-coords*) *cells*))))

(defun clear-cells()
  (let ((length (length (first (slot-value *cells* 'cells-matrix)))))
       (dotimes (i (length (slot-value *cells* 'cells-matrix)))
         (setf (nth i (slot-value *cells* 'cells-matrix)) (make-list length))))
  (stop-if-not-paused))

(defun go-if-paused()
  (if *paused* (toggle-pause)))

(defun stop-if-not-paused()
  (unless *paused* (toggle-pause)))

(defmethod glut:keyboard ((w cube-window) key x y)
  (declare (ignore x y))

  (move-chooser (case key 
                  (#\w :up)
                  (#\s :down)
                  (#\d :right)
                  (#\a :left)))

  (add-row (case key
             (#\t :up)
             (#\b :down)
             (#\l :left)
             (#\r :right)))

  (remove-row (case key
                (#\T :up)
                (#\B :down)
                (#\L :left)
                (#\R :right)))

  (if (equal key #\Esc)
    (glut:destroy-current-window)
    (progn
      (case key
        (#\f (setf *fix-coords* (not *fix-coords*)))
        ((#\+ #\=) (setf *generation-timer-timeout* (/ *generation-timer-timeout* 2)) (go-if-paused))
        (#\- (setf *generation-timer-timeout* (* *generation-timer-timeout* 2))(go-if-paused))
        (#\g (set-glider))
        (#\n (update-generation))
        (#\c (clear-cells))
        (#\Space (toggle-pause))
        (#\Return (setxycell-by-chooser-coords))
        (#\Esc )
        (t (format t "Unknown key pressed ~A~%" key)))
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
  (let ((glut:*run-main-loop-after-display* nil)(*cells* (make-live-cells '((nil t nil) (nil nil t) (t   t   t)))))
      (run-game-of-life)
    (glut:main-loop)))
