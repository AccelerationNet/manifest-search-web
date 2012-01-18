;; -*- lisp -*-

(cl:defpackage :manifest-search-web
  (:use :cl :cl-user :iterate :manifest-search )
  (:shadowing-import-from :alexandria :ensure-list)
  (:shadowing-import-from :buildnode :with-html-document)
  (:shadowing-import-from :manifest-search :doc-value :find-doc-by-key :docs-for-term)
  (:export ))

(in-package :manifest-search-web)
(cl-interpol:enable-interpol-syntax)

(defvar *acceptor* nil "the hunchentoot acceptor")

(defun resource-path (path)
  "looks up path relative to whereever this asdf system is installed.  Returns a truename"
  (truename (asdf:system-relative-pathname :manifest-search-web path)))

(defun start-server (&optional (port 8888))
  (unless *acceptor*
    (setf *acceptor*
          (make-instance 'hunchentoot:easy-acceptor
                         :port port
                         :document-root (resource-path "www")))
    (hunchentoot:start *acceptor*))
  ;; make a folder dispatcher the last item of the dispatch table
  ;; if a request doesn't match anything else, try it from the filesystem
  ;; (setf hunchentoot:*dispatch-table*
  ;;       (append hunchentoot:*dispatch-table*
  ;;               (list
  ;;                (hunchentoot:create-folder-dispatcher-and-handler "/" (resource-path "www"))))
  ;;       )
  )

(defun stop-server ()
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)
    T))

(defun json-search-results-fn (doc score)
  (json:as-array-member ()
    (json:with-object ()
      (iter (for n in (list :package :name :type :documentation))
        (json:as-object-member (n)
          (json:encode-json (doc-value doc n))))
      (when (eql :package (doc-value doc :type))
        (json:as-object-member (:readme)
          (json:encode-json (doc-value doc :readme))))
      (json:as-object-member (:score)
        (json:encode-json score)))))

(defun search-results-json (q n)
  (setf (hunchentoot:content-type*) "application/json")
  (when q
    (with-output-to-string (json:*json-output*)
      (json:with-array ()
        (search-manifest
         #?"DOCUMENTATION:(${q})"
         :n (or n 25)
         :result-fn #'json-search-results-fn)))))

(defun search-results-lisp (q n)
  (setf (hunchentoot:content-type*) "application/sexp")
  (princ-to-string
   (search-manifest-collecting
    #?"DOCUMENTATION:(${q})" :n n)))

(hunchentoot:define-easy-handler (search-handler :uri "/search")
    (q (n :parameter-type 'integer)
      (type :parameter-type #'symbol-munger:english->keyword))  
  (case type
    (:json (search-results-json q n))
    ((or :lisp :plist)
     (search-results-lisp q n))
    (T (search-view q n))))

(defun window (&optional (title "Search Lisp Documentation") &rest content)
  (xhtml:html ()
    (xhtml:head ()
      (xhtml:link `(:type :text/css :rel :stylesheet :href :style.css) )
      (xhtml:title () title))
    (xhtml:body ()
      (xhtml:h1 () title)
      content)))

(defmacro render-window ((&key (title "Quicklisp Documentation"))
                         &body body)
  `(buildnode:document-to-string
    (with-html-document
      (window ,title ,@body))))

(defun search-view (&optional search n)
  (render-window (:title (if search
                             "Common Lisp Documentation Search Results"
                             "Search Common Lisp Documentation"))
    (xhtml:div `(:class "search")
      (xhtml:form ()
        (xhtml:input `(:type "text" :value ,search :name "q"))
        (xhtml:button () "Search"))
      (when search
        (let ((results
                (search-manifest-collecting
                 #?"DOCUMENTATION:(${search})" :n n)))
         (xhtml:div '(:class "results")
            (mapcar #'search-result-fn results)
           ))))))

(defun sr-val (r name)
  "get a value from the search results"
  (etypecase r
    (montezuma:document (doc-value r name))
    (list (getf r name))))

(defun search-result-fn (d)
  (let ((package (sr-val d :package))
        (name (sr-val d :name))
        (type (sr-val d :type)))
    (xhtml:div '(:class "res")
      (xhtml:h4 ()
        (xhtml:a `(:href ,#?"/package?p=${package}") package)
        #?" ${name} <${type}>")
      (xhtml:div '(:class "description")
        (sr-val d :documentation)))))


(defun package-view (package)
  (let ((doc (find-doc-by-key nil package :package))
        (items (docs-for-term :package package)))
    (render-window
        (:title #?"Documentation For ${package}") 
      (xhtml:div '(:class "package")
        (xhtml:div '(:class "documentation")
          (doc-value doc :documentation))
        (xhtml:div '(:class "readme")
          (doc-value doc :readme))
        (xhtml:div '(:class "members")
          (iter (for i in items)
            (collect (search-result-fn i))))))))

(hunchentoot:define-easy-handler (%package-view :uri "/package")
    ((p :parameter-type #'symbol-munger:english->keyword))
  (package-view p))