;;;; futile.lisp

(in-package #:futile)

(defun range (first &optional (second nil) (step 1))
  "Creates a list of numbers in a range"
  (macrolet ((for (second word first)
               `(loop :for x :from ,second ,word ,first :by step
                      :collect x)))
    (cond ((and second (> second first)) (for first to second))
          (second (for first downto second))
          (t (for 0 to first)))))

(defun flatten (xs &optional (acc '()))
  "Flattens a nested list"
  (cond
    ((null xs)
     acc)
    ((atom xs)
     (cons xs acc))
    (t
     (flatten (car xs) (flatten (cdr xs) acc)))))

(defmacro ->> (&rest r)
  "Tail-led pipelining/threading"
  (reduce (lambda (o i)
            `(,@i ,o)) r))

(defmacro -> (&rest r)
  "Elixir-style front-running pipelining/threading"
  (reduce (lambda (o i)
            `(,(car i) ,o ,@(cdr i))) r))

(defmacro defarity (sym &rest bodies)
  "Clojure-style arity overloading. Useage: (defarity function-name ((x y) (+ x y)) ((x) x) (t other-stuff))"
  (let ((args (gensym))
        (acc '()))
    `(defun ,sym (&rest ,args)
       (case (length ,args)
         ,@(loop :for body in (remove-if (lambda (body)
                                           (eq (car body) t)) bodies)
                 :collect 
                 (unless (eq (first body) t)
                   `(,(length (first body))
                     (apply (lambda ,(first body) ,@(rest body)) ,args))))
         
         ,@(loop for body in bodies
                 do
                    (if (eq (car body) t)
                        (setq acc (rest body))))

         ,(if (not (null acc))
              `(otherwise ,@acc)
              `(otherwise (error "No behaviour specified for this arity")))))))


(defmacro condp (test predicate &body forms)
  `(cond
     ,@(mapcar (lambda (cc)
                 `((,test ,(first cc) ,predicate) (progn
                                                    ,@(rest cc))))
               (butlast forms))
     (t
      ,@(last forms))))


(defun read-file (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defun empty? (x)
  (not (to-list x)))

(defun to-string (ch-seq)
  (f:convert 'string ch-seq))

(defun to-list (s)
  (f:convert 'cl:list s))

(defun to-seq (s)
  (f:convert 'cl:seq s))


(defun partition (seq n)
  (let ((seq (to-list seq)))
    (labels ((seq-split (seq n &optional acc orig-n)
               (cond ((zerop (length seq)) (nreverse acc))
                     ((zerop n) (seq-split seq 
                                           orig-n
                                           (cons (subseq seq 0 0) acc)
                                           orig-n))
                     (t (seq-split (subseq seq 1)
                                   (1- n) 
                                   (cons (concatenate (class-of seq) 
                                                      (if acc (car acc) (subseq seq 0 0))
                                                      (cl:list (elt seq 0)))
                                         (cdr acc))
                                   orig-n)))))
      (seq-split seq n nil n))))

(defun duplicates (coll &optional (acc '()) (found '()))
  "Returns a list of duplicate members. Can be paired with f:count !"
  (let ((coll (to-list coll)))
    (if (empty? coll)
        found
        (if (member (first coll) acc)
            (if (not (member (first coll) found))
                (duplicates (rest coll) acc (append found (cl:list (first coll))))
                (duplicates (rest coll) acc found))
            (duplicates (rest coll) (append acc (cl:list (first coll))) found)))))

(defun i-reduce (f acc coll &optional (i 0))
  "Reduces on a set, list, or seq"
  (let ((coll (to-seq coll)))
    (if (empty? coll)
        acc
        (i-reduce f (funcall f i acc (f:first coll)) (f:less-first coll) (cl:+ i 1)))))


(defun i-filter (f coll &optional (i 0))
  "Returns a list. Takes a set, list, seq"
  (let ((coll (to-seq coll)))
    (if (empty? coll)
        nil
        (if (funcall f i (f:first coll))
            (cons (f:first coll) (i-filter f (f:less-first coll) (cl:+ i 1)))
            (i-filter f (f:less-first coll) (cl:+ i 1))))))

(defun kv-reduce (f acc m)
  "kv-reduce on maps"
  (let ((coll (to-list m)))
    (labels ((aux (f acc coll)
               (if (empty? coll)
                   acc
                   (kv-reduce f (funcall f acc (car (first coll)) (cdr (first coll))) (rest coll)))))
      (aux f acc coll))))

(defun second (coll)
  (f:@ coll 2))

(defun less-nth (n coll)
  (if (empty? coll)
      coll
      (if (= n 0)
          coll
          (less-nth (cl:- n 1) (f:less-first coll)))))

(defun less-nth-last (n coll)
  (if (empty? coll)
      coll
      (if (= n 0)
          coll
          (less-nth-first (cl:- n 1) (f:less-last coll)))))

(defun @-in (coll ks)
  (if (empty? ks)
      coll
      (@-in (f:@ coll (f:first ks)) (rest ks))))


;; type-ccase
