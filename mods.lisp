
(in-package #:futile)

(defclass Modular ()
  ((mod
    :initarg :mod
    :accessor mod)
   (num
    :initarg :num
    :accessor num)))

(defmethod initialize-instance :after ((m Modular) &key)
  (with-slots (mod num) m
    (assert (< num mod))
    (assert (>= num 0))
    (assert (> mod 0))))

(defun make-mod (n r)
  (make-instance 'Modular :mod n :num r))

(defmacro def-bin-mod-method (sym op)
  "Define binary methods for combining modular numbers"
  `(progn
     (defmethod ,sym ((m1 Modular) (m2 Modular))
       (if (not (= (mod m1) (mod m2)))
           (error "Mods of ~A and ~A don't match" m1 m2) ;; You can only combine mod types with same modulo
           (make-instance 'Modular :mod (mod m1) :num (cl:mod (,op (num m1) (num m2)) (mod m1)))))
     (defmethod ,sym (scalar (m1 Modular))
       (make-instance 'Modular :mod (mod m1) :num (cl:mod (,op (num m1) scalar) (mod m1))))
     (defmethod ,sym ((m1 Modular) scalar)
       (make-instance 'Modular :mod (mod m1) :num (cl:mod (,op scalar (num 1)) (mod m1))))))


(def-bin-mod-method + cl:+)
(def-bin-mod-method - cl:-)
(def-bin-mod-method * cl:*)
(def-bin-mod-method / cl:/)

(defmethod print-object ((m1 Modular) stream)
  (print-unreadable-object (m1 stream :type t)
    (with-accessors ((num num)
                     (mod mod))
        m1
      (format stream "~a mod ~a" num mod))))
