(defpackage #:gol-asd
  (:use :cl :asdf))

(in-package :gol-asd)

(defsystem :gol
           :author "Olexiy Zamkoviy <olexiy.z@gmail.com>"
           :version "0.0.1"
           :description "Conway's Game of life backend implementation"
           :components 
           ((:file "gol")))

(defsystem :gol-gl-frontend
           :author "Olexiy Zamkoviy <olexiy.z@gmail.com>"
           :version "0.0.1"
           :depends-on (:cl-opengl :cl-glu :cl-glut :gol)
           :description "Conway's Game of life opengl frontend"
           :components 
           ((:file "gol-gl")))

(defsystem :gol-tests
           :author "Olexiy Zamkoviy <olexiy.z@gmail.com>"
           :version "0.0.1"
           :depends-on (:lift :f-underscore :gol :metatilities)
           :components 
           ((:file "gol-tests")))
