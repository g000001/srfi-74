;;;; srfi-74.asd

(cl:in-package :asdf)

(defsystem :srfi-74
  :serial t
  :depends-on (:fiveam
               :mbe
               :srfi-5
               :srfi-23
               :srfi-26
               :srfi-60
               :srfi-66)
  :components ((:file "package")
               (:file "util")
               (:file "srfi-74")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-74))))
  (load-system :srfi-74)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-74.internal :srfi-74))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
