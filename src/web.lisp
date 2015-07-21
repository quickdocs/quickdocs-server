(in-package :cl-user)
(defpackage quickdocs-server.web
  (:use :cl
        :caveman2
        :quickdocs-server.config
        :quickdocs-server.view
        :quickdocs-server.db
        :datafly
        :sxql
        :quickdocs-database)
  (:import-from :quickdocs-server.search
                :search-projects
                :download-stats)
  (:import-from :lack.component
                :call)
  (:import-from :datafly
                :*connection*
                :connect-cached)
  (:export :*web*))
(in-package :quickdocs-server.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(defmethod lack.component:call :around ((app <web>) env)
  (let ((datafly:*connection*
          (apply #'datafly:connect-cached (cdr (assoc :maindb (config :databases))))))
    (call-next-method)))
(clear-routing-rules *web*)


;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"
          (list :ql-dist-version (preference "ql-dist-version")
                :app-env (appenv))))

@route GET "/:project-name/"
(defun project-page (&key project-name)
  (let ((project (retrieve-project project-name)))
    (unless project
      (throw-code 404))

    (let ((dependencies
            (mapcar (lambda (dep)
                      (list :name (project-name dep)
                            :description (project-description dep)))
                    (project-dependencies project)))
          (dependees
            (mapcar (lambda (dep)
                      (list :name (project-name dep)
                            :description (project-description dep)))
                    (project-dependees project))))
      (render #P"project.html"
              `(:project-name ,project-name
                :ql-dist-version ,(project-ql-dist-version project)
                :homepage    ,(project-homepage-url* project)
                :repos-url   ,(project-repos-url project)
                :archive-url ,(project-archive-url project)
                :readme ,(let ((readme (project-readme project)))
                           (when readme (project-readme-converted readme)))
                :authors ,(project-authors project)
                :maintainers ,(project-maintainers project)
                :licenses ,(project-licenses project)
                :categories ,(project-categories project)
                :dependencies-count ,(length dependencies)
                :dependencies ,dependencies
                :dependees-count ,(length dependees)
                :dependees ,dependees)))))

@route GET "/:project-name/api"
(defun project-api-reference (&key project-name)
  (let ((project (retrieve-project project-name)))
    (unless project
      (throw-code 404))

    (render #P"api.html"
            `(:project-name ,project-name
              :ql-dist-version ,(project-ql-dist-version project)
              :homepage    ,(project-homepage-url* project)
              :repos-url   ,(project-repos-url project)
              :archive-url ,(project-archive-url project)
              :systems ,(mapcar (lambda (system)
                                  (list :name (system-name system)
                                        :description (system-description system)
                                        :packages
                                        (mapcar (lambda (package)
                                                  (setf (getf package :symbols)
                                                        (mapcar (lambda (symbol)
                                                                  (setf (getf symbol :type)
                                                                        (string-downcase (getf symbol :type)))
                                                                  (setf (getf symbol :name)
                                                                        (quickdocs-serializer:symb-name
                                                                         (getf symbol :name)))
                                                                  symbol)
                                                                (getf package :symbols)))
                                                  package)
                                                (system-extracted-info-packages
                                                 (system-extracted-info system)))))
                                (project-systems project))))))

@route GET "/search"
(defun search-page (&key |q|)
  (let ((projects (search-projects |q| (preference "ql-dist-version"))))

    (render #P"search.html"
            (list
             :projects (mapcar (lambda (project)
                                 (list :name (project-name project)
                                       :release-version (project-release-version project)
                                       :description (project-description project)
                                       :download-count (gethash (project-name project)
                                                                (quickdocs-server.search:download-stats))))
                               projects)
             :query |q|))))

@route GET "/:project-name"
(defun redirect-to-project (&key project-name)
  (redirect (format nil "/~A/" (quri:url-encode project-name)) 301)
  "")


;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
