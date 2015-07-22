(in-package :cl-user)
(defpackage quickdocs-server.view
  (:use :cl)
  (:import-from :quickdocs-server.config
                :*template-directory*)
  (:import-from :caveman2
                :*response*
                :response-headers)
  (:import-from :djula
                :add-template-directory
                :compile-template*
                :render-template*
                :*djula-execute-package*)
  (:import-from :datafly
                :encode-json)
  (:export :render
           :render-json))
(in-package :quickdocs-server.view)

(djula:add-template-directory *template-directory*)

(defparameter *template-registry* (make-hash-table :test 'equal))

(defun render (template-path &optional env)
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (princ-to-string template-path)))
      (setf (gethash template-path *template-registry*) template))
    (apply #'djula:render-template*
           template nil
           env)))

(defun render-json (object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (encode-json object))


;;
;; Execute package definition

(defpackage quickdocs-server.djula
  (:use :cl)
  (:import-from :quickdocs-server.config
                :config
                :appenv
                :developmentp
                :productionp)
  (:import-from :caveman2
                :url-for))

(setf djula:*djula-execute-package* (find-package :quickdocs-server.djula))


;;
;; Custom filters

(djula::def-filter :symbol (it)
  (quickdocs-serializer:symb-name it))

(djula::def-filter :symbol-with-package (it)
  (format nil "~A:~A"
          (quickdocs-serializer:symb-package it)
          (quickdocs-serializer:symb-name it)))

(djula::def-filter :lambda-list (it)
  (labels ((maptree (fn obj)
             (if (consp obj)
                 (cons (maptree fn (car obj))
                       (and (cdr obj)
                            (maptree fn (cdr obj))))
                 (funcall fn obj)))
           (princ-for-html (item)
             (djula::escape-for-html (format nil "~(~A~)" item))))
    (maptree (lambda (item)
               (typecase item
                 (quickdocs-serializer:symb
                  (format nil "<span class=\"symbol-name\">~A</span>"
                          (princ-for-html
                           (quickdocs-serializer:symb-name item))))
                 ((and symbol
                       (not keyword))
                  (if (eq (symbol-package item) (find-package :cl))
                      (format nil "<span class=\"keyword\">~A</span>"
                              (princ-for-html item))
                      (format nil "<span class=\"symbol-name\">~A</span>"
                              (princ-for-html item))))
                 (otherwise (princ-for-html item))))
             it)))
