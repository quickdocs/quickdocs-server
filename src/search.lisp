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

(defvar *ql-download-stats* nil)

@export
(defun download-stats ()
  (or *ql-download-stats*
      (loop with hash = (make-hash-table :test 'equal)
            for row in (retrieve-all
                        (select :*
                          (from :quicklisp_download_stats)))
            do (setf (gethash (getf row :project-name) hash)
                     (getf row :download-count))
            finally
               (return (setf *ql-download-stats* hash)))))

(defun sort-by-download-count (a b)
  (let ((stats (download-stats)))
    (> (gethash (project-name a) stats 0)
       (gethash (project-name b) stats 0 ))))

@export
(defun search-projects (query &optional (ql-dist-version
                                         (preference "ql-dist-version")))
  (if (and query
           (/= 0 (length query)))
      (remove-duplicates
       (append
        (ensure-list (search-exact-project query ql-dist-version))
        (sort (search-by-categories query ql-dist-version) #'sort-by-download-count)
        (sort (search-by-name query ql-dist-version) #'sort-by-download-count)
        (sort (search-by-description query ql-dist-version) #'sort-by-download-count))
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
                  (where (:like :category (format nil "%~(~A~)%" query))))))
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
  (retrieve-all
   (select :project.*
     (from :project)
     (left-join :cliki :on (:= :project.name :cliki.project_name))
     (left-join :repos_info :on (:= :project.name :repos_info.project_name))
     (where `(:and (:= :ql_dist_version ,ql-dist-version)
                   (:or (:like :repos_info.description ,(format nil "%~A%" query))
                        (:like :body ,(format nil "%~A%" query))))))
   :as 'project))
