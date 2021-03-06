(cl:in-package "https://github.com/g000001/srfi-74#internals")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'eq?) #'eq)
  (setf (fdefinition 'integer?) #'integerp)
  (setf (fdefinition 'list?) #'listp)
  (setf (fdefinition 'negative?) #'minusp)
  (setf (fdefinition 'null?) #'null)
  (setf (fdefinition 'pair?) #'consp)
  (setf (fdefinition 'positive?) #'plusp)
  (setf (fdefinition 'zero?) #'zerop)
  (setf (fdefinition 'vector-length) #'length)
  (setf (fdefinition 'vector?) #'vectorp)
  (setf (fdefinition 'procedure?) #'functionp)
  (setf (fdefinition 'exact?) #'rationalp)
  (setf (fdefinition 'even?) #'evenp)
  (setf (fdefinition 'real?) #'realp)
  (setf (fdefinition 'newline) #'terpri)
  (setf (fdefinition 'display) #'princ)
  (setf (fdefinition 'remainder)  #'rem)
  )


(defmacro set! (var val)
  `(setq ,var ,val))


(declaim (inline list-tail vector-set! list-ref vector->list list->vector
                 quotient))


(defun quotient (x y)
  (values (truncate x y)))


(defun list-tail (list k)
  (nthcdr k list))


(defun list-ref (list k)
  (nth k list))


(defun vector-set! (vec index val)
  (setf (aref vec index) val))


(defun vector->list (vec)
  (coerce vec 'list))


(defun list->vector (list)
  (coerce list 'vector))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-proper-lambda-list (list)
    (typecase list
      (list (if (tailp () list)
                list
              (cl:let ((last (last list)))
                `(,@(butlast list)
                  ,(car last)
                  cl:&rest
                  ,(cdr last)))))
      (symbol `(cl:&rest ,list)))))


(defmacro lambda (args &rest body)
  `(cl:lambda ,(to-proper-lambda-list args)
     ,@body))


(defmacro letrec ((&rest binds) &body body)
  `(let (,@(mapcar (cl:lambda (x)
                     `(,(car x) #'values))
             binds))
     (declare (optimize (space 3)))
     (labels (,@(remove nil
                  (mapcar (cl:lambda (x &aux (name (car x)))
                            `(,name
                               (&rest args)
                               (apply ,name args)))
                          binds)))
       (declare (optimize (debug 0) (space 3)))
       (psetq ,@(apply #'append binds))
       ,@body)))


(defmacro define-function (name-args &body body)
  (if (consp name-args)
      (destructuring-bind (name . args)
                          name-args
        `(defun ,name ,(to-proper-lambda-list args)
           ,@body))
      `(progn
         (setf (fdefinition ',name-args)
               ,(car body)))))


(declaim (inline vector-ref))


(defun vector-ref (vec k)
  (svref vec k))


(declaim (inline modulo))


(defun modulo (x y)
  (mod x y))


(defmacro begin (&body body)
  `(progn ,@body))


(declaim (inline make-vector))


(defun make-vector (size &optional (init 0))
  (cl:make-array size                   ;***
                 :initial-element init
                 :adjustable nil
                 :fill-pointer nil))


(declaim (inline string-append))


(defun string-append (&rest strings)
  (format nil "~{~A~}" strings))


(declaim (inline number->string))


(defun number->string (num)
  (write-to-string num))


(defmacro dolex ((&rest varlist) endlist &body body)
  (let* ((vars (mapcar (lambda (v)
                         (if (consp v) (car v) v) )
                       varlist ))
         (binds (mapcar (lambda (b)
                          (if (consp b)
                              (destructuring-bind (var &optional init next)
                                                  b
                                (if next
                                    `(,var ,init
                                           (let (,@(mapcar (lambda (x)
                                                             (list x x) )
                                                     vars ))
                                             (declare (ignorable ,@vars))
                                             ,next ))
                                    `(,var ,init)))
                              (list b nil) ))
                        varlist )))
    `(cl:do ,binds ,endlist ,@body) ))


;;; *EOF*
