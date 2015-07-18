(in-package :cl-user)
(defpackage quickdocs-server-asd
  (:use :cl :asdf))
(in-package :quickdocs-server-asd)

(defsystem quickdocs-server
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:clack
               :lack
               :caveman2
               :envy
               :cl-ppcre
               :quri

               :quickdocs-database

               ;; for @route annotation
               :cl-syntax-annot

               ;; HTML Template
               :djula

               ;; for DB
               :datafly
               :sxql)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view" "search"))
                 (:file "view" :depends-on ("config"))
                 (:file "search")
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description "Web application running at quickdocs.org.")
