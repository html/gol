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


(defpackage :gol-tests
  (:use #:cl #:lift #:f-underscore #:metatilities)
  (:export :test-gol))

(require :gol)

(in-package :gol-tests)

(deftestsuite gol-suite()
  ())

(defmacro set-sensible-suite ()
  "Set up a sensible testsuite to use as the testsuite for addtest
forms that may not have a suite defined in-file, in the file in which
I am expanded.  Likely to work only at toplevel."
  (let ((inner-part
	 `(let ((last-set-suite lift::*current-testsuite-name*))
	    (unless (and file
			 (string-contains-p
			  (symbol-name last-set-suite) (pathname-name file)))
	      (setf lift::*current-testsuite-name* 'gol-suite)))))
    `(progn
       (eval-when (:compile-toplevel)
	 (let ((file *compile-file-pathname*))
	   ,inner-part))
       (eval-when (:load-toplevel :execute)
	 (let ((file *load-truename*))
	   ,inner-part)))))

(defmacro deftest (name form &rest values)
  "Define a test in an appropriate testsuite called NAME, ensuring
that FORM's values and the literal VALUES are equal."
  `(progn
     (set-sensible-suite)
     (addtest ,name
       (ensure-same ,form ,(if (typep values '(cons t null))
			       `',(first values)
			       `(values . ,(mapcar (f_ `',_) values)))))))

(defun test-gol (&optional (verbose t))
  "Call this function to run all unit tests defined in 'gol-tests'
package. This function tests gol in a clean environment. See
'with-test-environment' for more details.

Pass NIL as the optional arg to just return the results instead of
DESCRIBE-ing them."
  ;; XXX better results combination
  (let ((results (list (run-tests :suite 'gol-suite))))
    (when verbose
      (mapc #'describe results))
    (values-list results)))

(defun m(cells)
  (slot-value (gol:make-live-cells cells) 'cells-matrix))

(deftest xycell-1
         (gol:xycell 0 0 nil)
         nil)

(deftest xycell-2
         (gol:xycell 0 0 '((t)))
         t)

(deftest live-neighbours-count-1
         (gol:live-neighbours-count 
           1 1
           '((t t t)
             (t t t)
             (t t t)))
         8)

(deftest live-neighbours-count-2
         (gol:live-neighbours-count 
           0 0
           '((t t t)
             (t t t)
             (t t t)))
         3)

(deftest live-neighbours-count-3
         (gol:live-neighbours-count 
           0 1
           '((t t t)
             (t t t)
             (t t t)))
         5)

(deftest live-neighbours-count-4
         (gol:live-neighbours-count 
           1 0
           '((nil nil nil)
             (t   t   t)
             (nil nil nil)))
         3)
         
(deftest live-neighbours-count-5
         (gol:live-neighbours-count 
           0 0
           '((nil nil nil)
             (t   t   t)
             (nil nil nil)))
         2)

(deftest cell-value-must-alive
         (gol:cell-value 
           0 0
           '((nil t t)
             (t t t)
             (t t t)))
         t)

(deftest cell-value-must-stay-old
         (gol:cell-value 
           0 0
           '((1 nil t)
             (t t  t)
             (t t  t)))
         1)

(deftest cell-value-must-die-due-to-loneliness
         (gol:cell-value 
           0 0
           '((t nil t)
             (t nil t)
             (t t t)))
         nil)

(deftest cell-value-must-die
         (gol:cell-value 
           1 1
           '((t t t)
             (t t t)
             (t t t)))
         nil)

(addtest 'next-generation-1
         (ensure (equal (gol:next-generation 
           '((t   nil t)
             (nil nil nil)
             (t   nil t)))
           '((nil nil nil)
             (nil nil nil)
             (nil nil nil)))))

(addtest 'next-generation-2
         (ensure (equal (gol:next-generation 
           '((t   t   t)
             (t   t   t)
             (t   t   t)))
           '((t   nil t)
             (nil nil nil)
             (t   nil t)))))

(addtest 'next-generation-3
         (ensure (equal (gol:next-generation 
           '((nil   t   nil)
             (nil   t   nil)
             (nil   t   nil)))
           '((nil   nil nil)
             (t     t   t)
             (nil   nil nil)))))

(addtest 'list-or-live-cells-instance-to-list-1
         (ensure (equal 
                   (gol:list-or-live-cells-instance-to-list (gol:make-live-cells '((1 2 3))))
                   '((1 2 3)))))

(deftest xycell-with-live-cells-instance-1
         (gol:xycell 0 0 (gol:make-live-cells))
         nil)

; Start here

(deftest xycell-with-live-cells-instance-2
         (gol:xycell 0 0 (gol:make-live-cells '((t))))
         t)

(deftest live-neighbours-count-1
         (gol:live-neighbours-count 
           1 1
           (gol:make-live-cells '((t t t)
             (t t t)
             (t t t))))
         8)

(deftest live-neighbours-count-2
         (gol:live-neighbours-count 
           0 0
           (gol:make-live-cells '((t t t)
             (t t t)
             (t t t))))
         3)

(deftest live-neighbours-count-3
         (gol:live-neighbours-count 
           0 1
           (gol:make-live-cells '((t t t)
             (t t t)
             (t t t))))
         5)

(deftest live-neighbours-count-4
         (gol:live-neighbours-count 
           1 0
           (gol:make-live-cells '((nil nil nil)
             (t   t   t)
             (nil nil nil))))
         3)
         
(deftest live-neighbours-count-5
         (gol:live-neighbours-count 
           0 0
           (gol:make-live-cells '((nil nil nil)
             (t   t   t)
             (nil nil nil))))
         2)

(deftest cell-value-must-alive
         (gol:cell-value 
           0 0
           (gol:make-live-cells '((nil t t)
             (t t t)
             (t t t))))
         t)

(deftest cell-value-must-stay-old
         (gol:cell-value 
           0 0
           (gol:make-live-cells '((1 nil t)
             (t t  t)
             (t t  t))))
         1)

(deftest cell-value-must-die-due-to-loneliness
         (gol:cell-value 
           0 0
           (gol:make-live-cells '((t nil t)
             (t nil t)
             (t t t))))
         nil)

(deftest cell-value-must-die
         (gol:cell-value 
           1 1
           (gol:make-live-cells '((t t t)
             (t t t)
             (t t t))))
         nil)

;;; All tests below need to be  fixed
(addtest 'next-generation-1
         (let* ((cells (gol:make-live-cells 
             '((t   nil t)
             (nil nil nil)
             (t   nil t))))
               (next-gen (gol:next-generation cells)))
         #+l(ensure (equal (print (slot-value cells 'gol:cells-matrix))
           '((nil nil nil)
             (nil nil nil)
             (nil nil nil))))))

(addtest 'next-generation-2
         (ensure (equal (gol:next-generation 
           '((t   t   t)
             (t   t   t)
             (t   t   t)))
           '((t   nil t)
             (nil nil nil)
             (t   nil t)))))

(addtest 'next-generation-3
         (ensure (equal (gol:next-generation 
           '((nil   t   nil)
             (nil   t   nil)
             (nil   t   nil)))
           '((nil   nil nil)
             (t     t   t)
             (nil   nil nil)))))
