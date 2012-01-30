;; -*- lisp -*-

(cl:defpackage :manifest-search-web
  (:use :cl :cl-user :iterate :manifest-search )
  (:shadowing-import-from :alexandria :ensure-list)
  (:shadowing-import-from :buildnode :with-html-document)
  (:shadowing-import-from :manifest-search :doc-value :find-doc-by-key :docs-for-term)
  (:export *web-root* start-server stop-server resource-path))

(in-package :manifest-search-web)
(cl-interpol:enable-interpol-syntax)

(defvar *acceptor* nil "the hunchentoot acceptor")
(defvar *web-root* (truename (asdf:system-source-directory :manifest-search-web)))

(defun resource-path (path)
  "looks up path relative to whereever this asdf system is installed.  Returns a truename"
  (truename (merge-pathnames path *web-root* )))

(defun start-server (&optional (port 8888))
  (unless *acceptor*
    (setf *acceptor*
          (make-instance 'hunchentoot:easy-acceptor
                         :port port
                         :address "127.0.0.1"
                         :document-root (resource-path "www")))
    (hunchentoot:start *acceptor*)))

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
         #?"DOCUMENTATION:(${q}) README:(${q})"
         :n (or n 50)
         :result-fn #'json-search-results-fn)))))

(defun search-results-lisp (q n)
  (setf (hunchentoot:content-type*) "application/sexp")
  (princ-to-string
   (search-manifest-collecting
    #?"DOCUMENTATION:(${q}) README:(${q})" :n n)))

(hunchentoot:define-easy-handler (search-handler :uri "/search")
    (q (n :parameter-type 'integer)
      (type :parameter-type #'symbol-munger:english->keyword))  
  (case type
    (:json (search-results-json q n))
    ((or :lisp :plist)
     (search-results-lisp q n))
    (T (search-view q n))))

(defun window (&optional (title "Search Lisp Documentation")
                 (body-class "")
               &rest content)
  (xhtml:html ()
    (xhtml:head ()
      (xhtml:link `(:type :text/css :rel :stylesheet :href :style.css) )
      (xhtml:title () title))
    (xhtml:body `(:class ,body-class)
      (xhtml:h1 () title)
      (xhtml:a '(href "/") "Home")
      (xhtml:div '(:class "content")
        content))))

(defmacro render-window ((&key (title "Quicklisp Documentation")
                            class)
                         &body body)
  `(buildnode:document-to-string
    (with-html-document
      (window ,title ,class ,@body))))

(defun %search-form (&optional q)
  (xhtml:form `(:action "/search" :method :get)
    (xhtml:input `(:type "text" :value ,(or q "") :name "q"))
    (xhtml:button () "Search")))

(defun search-view (&optional q n)
  (render-window (:title (if q
                             "Common Lisp Documentation Search Results"
                             "Search Common Lisp Documentation"))
    (xhtml:div `(:class "search")
      (%search-form q)
      (when q
        (let ((results
                (search-manifest-collecting
                 #?"DOCUMENTATION:(${q}) README:(${q})" :n n)))
         (xhtml:div '(:class "results")
            (mapcar #'search-result-fn results)))))))

(defun sr-val (r name)
  "get a value from the search results"
  (etypecase r
    (montezuma:document (doc-value r name))
    (list (getf r name))))

(defun shorten-string (s &optional (length 512) (ellipses "...")
                         &aux (slen (length s)) )
  "Make a long string shorter"
  (cond
    ((null length) s)
    ((= slen 0) nil)
    ((<= slen length) s)
    (T (concatenate 'string (subseq s 0 (- length (length ellipses)))
		    ellipses))))

(defun search-result-fn (d &optional (shorten-to 512))
  (let* ((name (sr-val d :name))
         (type (intern (sr-val d :type) :keyword))
         (package (case type
                    (:package name)
                    (t (sr-val d :package))) )
         (arglist (or (sr-val d :arglist) ""))
         (url #?"/package?p=${package}"))
    (xhtml:div `(:class "res" :id ,name)
      (xhtml:h4 ()
        (xhtml:a `(:href ,url) package)
        (xhtml:a `(:href ,#?"${url}#${name}")
          #?" ${name} ${arglist} ")
        #?"<${type}>")
      (xhtml:div '(:class "description")
        (case type
          (:package
           (or (shorten-string (sr-val d :readme) shorten-to)
               (shorten-string (sr-val d :documentation) shorten-to)))
          (t (shorten-string (sr-val d :documentation) shorten-to)))))))

(defun display-item-fn (d)
  (search-result-fn d nil))

(defun package-view (package)
  (let ((doc (package-document package))
        (items (docs-for-term :package package)))
    (render-window
        (:title #?"Documentation For ${package}") 
      (xhtml:div '(:class "package")
        (xhtml:div '(:class "documentation")
          (doc-value doc :documentation))
        (xhtml:div '(:class "readme")
          (doc-value doc :readme))
        (xhtml:div '(:class "members")
          (mapcar #'display-item-fn items))))))

(hunchentoot:define-easy-handler (%package-view :uri "/package")
    ((p :parameter-type #'symbol-munger:english->keyword))
  (package-view p))

(defun welcome-view ()
  (render-window (:title "Common Lisp Documentation Search Engine"
                   :class "welcome")
    (xhtml:div '()
      (%search-form)
      (xhtml:p ()
        "Here you can search the doc strings of all packages that are quicklisp loadable. ")
      (xhtml:p `(:class "about")
        "The source for this website is at "
        (xhtml:a '(href "https://github.com/AccelerationNet/manifest-search-web")
          "manifest-search-web")
        " with its sister project " 
        (xhtml:a '(href "https://github.com/AccelerationNet/manifest-search")
          "manifest-search. ")
        "Inspired by and utilizing "
        (xhtml:a '(href "https://github.com/gigamonkey/manifest")
          "manifest. ")
        (xhtml:a '(href "https://github.com/gigamonkey/manifest"))
        "This project utilizes "
        (xhtml:a '(href "http://code.google.com/p/montezuma/") "montezuma")
        ", a common lisp port of lucene. Searches are performed using a subset of
         lucene syntax. "
        (xhtml:a '(href "http://l1sp.org")
          "Also see l1sp.org for a different kind of common lisp documentation search."))
      (xhtml:p '()
        (xhtml:h4 () "/search")
        "Searches common lisp docstrings"
        (xhtml:ul '(:class :parameters)
          (xhtml:li '(:class "n")
            "The \"n\" paramter controls the number of results (default 50)")
          (xhtml:li '(:class "q")
            "The \"q\" paramter is the search to be returned")
          (xhtml:li '(:class "type")
            "The \"type\" paramter is the format of the search
        results.  Defaults to html but also accepts json and lisp.
        Lisp returns a list of plists of search results")))
      (xhtml:p '()
        (xhtml:h4 () "/package")
        "Displays the documentation for a given package (and its contents)"
        (xhtml:ul '(:class :parameters)
          (xhtml:li '(:class "p")
            "The \"p\" paramter controls the package documentation to display")))
      (xhtml:p '(:class "issues")
        (xhtml:a '(href "https://github.com/AccelerationNet/manifest-search-web")
          "Please report bugs or suggest improvements at the github project page.")))))

(hunchentoot:define-easy-handler (%welcome :uri "/")
    ()
  (welcome-view))