;;;; package.lisp

(defpackage #:futile
  (:use #:cl)
  (:local-nicknames (#:f #:fset))
  (:shadow :+ :- :* :/ :second :string :list :mod))
