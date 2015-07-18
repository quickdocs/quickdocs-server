(in-package :cl-user)
(defpackage quickdocs-server.search
  (:use :cl
        :sxql
        :quickdocs-database)
  (:import-from :datafly
                :retrieve-all)
  (:import-from :alexandria
                :ensure-list))
(in-package :quickdocs-server.search)

(syntax:use-syntax :annot)

@export
(defun search-projects (query &optional (ql-dist-version
                                         (preference "ql-dist-version")))
  ;; TODO: sort
  (if (and query
           (/= 0 (length query)))
      (remove-duplicates
       (append
        (ensure-list (search-exact-project query ql-dist-version))
        (search-by-categories query ql-dist-version)
        (search-by-name query ql-dist-version)
        (search-by-description query ql-dist-version))
       :test #'eql
       :key #'project-id
       :from-end t)
      (search-all-projects ql-dist-version)))

@export
(defun search-all-projects (&optional (ql-dist-version
                                       (preference "ql-dist-version")))
  (retrieve-all
   (select :*
     (from :project)
     (where (:= :ql_dist_version ql-dist-version)))
   :as 'project))

(defun search-exact-project (query ql-dist-version)
  (retrieve-project query :ql-dist-version ql-dist-version))

(defun search-by-categories (query ql-dist-version)
  (let* ((scanner (ppcre:create-scanner
                   (format nil "\\b~A\\b" (ppcre:quote-meta-chars query)) :case-insensitive-mode t))
         (rows (retrieve-all
                (select (:project_name :category)
                  (from :cliki_project_category)
                  (where (:like :category (format nil "%~(~A~)%" query)))
                  (group-by :category))))
         (rows (remove-if-not
                (lambda (row)
                  (ppcre:scan scanner (getf row :category)))
                rows)))
    (if rows
        (retrieve-all
         (select :*
           (from :project)
           (where (:and (:= :ql_dist_version ql-dist-version)
                        (:in :name (mapcar (lambda (row)
                                             (getf row :project-name))
                                           rows)))))
         :as 'project)
        nil)))

(defun search-by-name (query ql-dist-version)
  (let ((queries (ppcre:split "\\s+" (string-downcase query))))
    (retrieve-all
     (select :*
       (from :project)
       (where `(:and (:= :ql_dist_version ,ql-dist-version)
                     ,@(mapcar
                        (lambda (query) `(:like :name ,(format nil "%~A%" query)))
                        queries))))
     :as 'project)))

(defun search-by-description (query ql-dist-version)
  (let ((queries (ppcre:split "\\s+" (string-downcase query))))
    (retrieve-all
     (select :project.*
       (from :cliki)
       (left-join :project :on (:= :cliki.project_name :project.name))
       (where `(:and (:= :ql_dist_version ,ql-dist-version)
                     ,@(mapcar
                        (lambda (query) `(:like (:lower :body) ,(format nil "%~A%" query)))
                        queries))))
     :as 'project)))
