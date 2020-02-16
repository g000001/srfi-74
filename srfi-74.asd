;;;; srfi-74.asd

(cl:in-package :asdf)


(defsystem :srfi-74
  :version "20200217"
  :description "SRFI 74 for CL: Octet-Addressed Binary Blocks"
  :long-description "SRFI 74 for CL: Octet-Addressed Binary Blocks
https://srfi.schemers.org/srfi-74"
  :author "Michael Sperber"
  :maintainer "CHIBA Masaomi"
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


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-74))))
  (let ((name "https://github.com/g000001/srfi-74")
        (nickname :srfi-74))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-74))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-74#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-74)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
