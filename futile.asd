;;;; futile.asd

(asdf:defsystem #:futile
  :description "Describe futile here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:fset)
  :components ((:file "package")
               (:file "general")
               (:file "mods")
               (:file "futile")))
