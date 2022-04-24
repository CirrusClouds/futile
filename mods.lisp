
(in-package #:futile)

(defclass Modular ()
  ((modulo
    :initarg :modulo
    :accessor modulo)
   (numba
    :initarg :numba
    :accessor numba)))

(defmethod initialize-instance :after ((m Modular) &key)
  (with-slots (modulo numba) m
    (assert (< numba modulo))
    (assert (> numba 0))
    (assert (> modulo 0))))

(setf *mod10* (make-instance 'Modular :modulo 10 :numba 3))

(defun make-mod (n r)
  (make-instance 'Modular :modulo n :numba r))

(defmacro def-bin-mod-method (sym op)
  "Define binary methods for combining modular numbers"
  `(progn
     (defmethod ,sym ((m1 Modular) (m2 Modular))
       (if (not (= (modulo m1) (modulo m2)))
           (error "Mods of ~A and ~A don't match" m1 m2) ;; You can only combine mod types with same modulo
           (make-instance 'Modular :modulo (modulo m1) :numba (mod (,op (numba m1) (numba m2)) (modulo m1)))))
     (defmethod ,sym (scalar (m1 Modular))
       (make-instance 'Modular :modulo (modulo m1) :numba (mod (,op (numba m1) scalar) (modulo m1))))
     (defmethod ,sym ((m1 Modular) scalar)
       (,sym scalar m1))))

(def-bin-mod-method + cl:+)
(def-bin-mod-method - cl:-)
(def-bin-mod-method * cl:*)
(def-bin-mod-method / cl:/)

(defmethod print-object ((m1 Modular) stream)
  (print-unreadable-object (m1 stream :type t)
    (with-accessors ((numba numba)
                     (modulo modulo))
        m1
      (format stream "~a mod ~a" numba modulo))))
