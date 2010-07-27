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

(export '(xycell cell live-neighbours-count cell-value next-generation do-cells setxycell))

(defparameter *can-out-of-bounds* nil)

(defun xcell(x cells)
    (if (and (>= x 0) (< x (length cells)))
             (nth x cells)))

(defun xycell(x y cells)
  (xcell x 
         (xcell y cells)))

(defmacro setxycell(x y cells value)
  `(setf (nth ,y 
             (nth ,x ,cells)) ,value))

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
  (loop for y from 0 to (1- (length cells))
        collect (loop for x from 0 to (1- (length (nth y cells)))
                      collect (cell-value x y cells))))
                      
(defmacro do-cells(cells &body body)
    `(dotimes (y (length ,cells))
        (dotimes (x (length (nth y ,cells)))
           (let ((cell (nth y (nth x ,cells))))
              (funcall (lambda (x y cell)
                         ,@body) x y cell)))))
