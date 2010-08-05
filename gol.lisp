;;;; Copyright (c) 2010 Olexiy Zamkoviy <olexiy.z@gmail.com>
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to
;;;; the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage :gol
  (:use #:cl))

(in-package :gol)

(export '(xycell cell live-neighbours-count cell-value next-generation do-cells setxycell list-or-live-cells-instance-to-list make-live-cells cells-matrix extreme-coord cells-x-out cells-y-out))

(defclass live-cells()
  ((cells-matrix
     :accessor :cells-matrix
     :initarg :cells-matrix)
   ;;;(can-expand-edges-p :initform t :initarg :can-expand-edges-p)
   (cells-x-out :initform 0 :accessor :cells-x-out)
   (cells-y-out :initform 0 :accessor :cells-y-out)))

(defmethod cell-by-coords(live-cells x y)
  (with-slots (cells-x-out cells-y-out) live-cells
      (let ((x (+ cells-x-out x))
            (y (+ cells-y-out y)))
        (xycell x y (slot-value live-cells 'cells-matrix)))))

(defmethod extreme-coord(live-cells way)
  (case way
    (:up (- (slot-value live-cells 'cells-y-out)))
    (:down (- (length (slot-value live-cells 'cells-matrix)) (slot-value live-cells 'cells-y-out)))
    (:left (- (slot-value live-cells 'cells-x-out)))
    (:right (- (length (first (slot-value live-cells 'cells-matrix))) (slot-value live-cells 'cells-x-out)))))


(defun make-live-cells(&optional cells)
  (make-instance 'live-cells :cells-matrix cells))

(defun list-or-live-cells-instance-to-list(list-or-live-cells-instance)
  (if (listp list-or-live-cells-instance)
      list-or-live-cells-instance
      (slot-value list-or-live-cells-instance 'cells-matrix)))

(defun xcell(x cells)
    (if (and (>= x 0) (< x (length cells)))
             (nth x cells)))

(defun xycell(x y cells)
  (if (listp cells)
      (xcell x 
             (xcell y cells))
      (cell-by-coords cells x y)))

(defmacro setxycell(x y cells value)
  `(let* ((listp (listp ,cells))
         (y (if listp ,y (+ ,y (slot-value ,cells 'cells-y-out))))
         (x (if listp ,x (+ ,x (slot-value ,cells 'cells-x-out)))))
         (setf (nth y 
             (nth x (if listp ,cells (slot-value ,cells 'cells-matrix)))) ,value)))

(defun neighbours-values(x y c)
  (list 
    (xycell (1+ x)  y c)
    (xycell  x      (1+ y) c)
    (xycell (1- x)  y c)
    (xycell  x      (1- y) c)
    (xycell (1+ x)  (1+ y) c)
    (xycell (1- x)  (1- y) c)
    (xycell (1- x)  (1+ y) c)
    (xycell (1+ x)  (1- y) c)))

(defun live-neighbours-count(x y cells)
  (count-if-not #'null (neighbours-values x y cells)))

(defun cell-value(x y cells)
  "Returns t if cell will be alive (t) or dead (nil) on next step"
  (let ((live-neighbours (live-neighbours-count x y cells)))
    (cond 
      ((= live-neighbours 3) t)
      ((or (< live-neighbours 2) (> live-neighbours 3)) nil)
      (t (xycell x y cells)))))

(defun next-generation(cells)
  (if (listp cells)
      (loop for y from 0 to (1- (length cells))
            collect (loop for x from 0 to (1- (length (nth y cells)))
                          collect (cell-value x y cells)))
      (progn 
        (let* ((matrix (slot-value cells 'cells-matrix))
              (newmatrix (mapcar 
                (lambda (list) (append (list nil) list (list nil)))
                (append 
                (list (make-list (length (first matrix))))
                matrix
                (list (make-list (length (first matrix))))))))
          (setf (slot-value cells 'cells-matrix) (next-generation newmatrix))
          (incf (slot-value cells 'cells-x-out) 1)
          (incf (slot-value cells 'cells-y-out) 1)
          (slot-value cells 'cells-matrix)))))
                      
(defmacro do-cells(cells &body body)
    `(let* ((listp (listp ,cells))
            (cells (if listp ,cells (slot-value ,cells 'cells-matrix)))
            (xt (if listp 0 (slot-value ,cells 'cells-x-out)))
            (yt (if listp 0 (slot-value ,cells 'cells-y-out))))
            (dotimes (y (length cells))
                (dotimes (x (length (nth y cells)))
                   (let ((cell (nth y (nth x cells))))
                      (funcall (lambda (x y cell)
                                 ,@body) (- x xt) (- y yt) cell))))))
