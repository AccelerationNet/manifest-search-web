;; -*- lisp -*-

(cl:defpackage :manifest-search-web
  (:use :cl :cl-user :iterate :manifest-search )
  (:shadowing-import-from :alexandria :ensure-list)
  (:shadowing-import-from :buildnode :with-html5-document)
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
  "Starts the server on specified port unless there is already a server running"
  (unless *acceptor*
    (setf *acceptor*
          (make-instance 'hunchentoot:easy-acceptor
                         :port port
                         :address "127.0.0.1"
                         :document-root (resource-path "www")))
    (hunchentoot:start *acceptor*)))

(defun stop-server ()
  "Stops the running server if it exists"
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)
    T))

(defun json-search-results-fn (doc score)
  "encodes a single search result as json"
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
  "Execute a query returning n results and write the whole thing out as a string
   also sets the hunchentoot content-type to be json"
  (setf (hunchentoot:content-type*) "application/json")
  (when q
    (with-output-to-string (json:*json-output*)
      (json:with-array ()
        (search-manifest
         #?"DOCUMENTATION:(${q}) README:(${q})"
         :n (or n 50)
         :result-fn #'json-search-results-fn)))))

(defun search-results-lisp (q n)
  "Run a query returning n results and write the results as a list of plists"
  (setf (hunchentoot:content-type*) "application/sexp")
  (princ-to-string
   (search-manifest-collecting
    #?"DOCUMENTATION:(${q}) README:(${q})" :n n)))

(hunchentoot:define-easy-handler (search-handler :uri "/search")
    (q (n :parameter-type 'integer)
      (type :parameter-type #'symbol-munger:english->keyword))
  (let ((n (or n 50)))
    (case type
      (:json (search-results-json q n))
      ((or :lisp :plist)
       (search-results-lisp q n))
      (T (search-view q n)))))

(defun window (&optional (title "Search Common Lisp Documentation")
                 (body-class "")
               &rest content)
  "Render the standard window frame this site will use"
  (html5:html ()
    (html5:head ()
      (html5:link `(:type :text/css :rel :stylesheet :href :style.css) )
      (html5:title () title))
    (html5:body `(:class ,body-class)
      (html5:header ()
        (html5:h1 () title)
        (html5:a '(href "/") "Home") " "
        (html5:a '(href "/html/index.html") "Static HTML Index"))
      (html5:section '(:class "content")
        content))))

(defmacro render-window ((&key (title "Quicklisp - Common Lisp Documentation")
                            class)
                         &body body)
  "Render the body in a window with the given body and class"
  `(buildnode:document-to-string
    (with-html5-document
      (window ,title ,class ,@body))))

(defun %search-form (&optional q)
  "A snippet of html that is the search form"
  (html5:form `(:action "/search" :method :get)
    (html5:input `(:type "text" :value ,(or q "") :name "q" :class "search"))
    (html5:button () "Search")))

(defun search-view (&optional q (n 50))
  "Render the search / search results page for a given query q and n number of results"
  (render-window (:title (if q
                             "Common Lisp Documentation Search Results"
                             "Search Common Lisp Documentation"))
    (html5:div `(:class "search")
      (%search-form q)
      (when q
        (let ((results
                (search-manifest-collecting
                 #?"DOCUMENTATION:(${q}) README:(${q})" :n n)))
         (html5:div '(:class "results")
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
  "Given a single search result produce html to display it"
  (let* ((name (sr-val d :name))
         (type (intern (sr-val d :type) :keyword))
         (package (case type
                    (:package name)
                    (t (sr-val d :package))) )
         (arglist (or (sr-val d :arglist) ""))
         (url #?"/html/${ (string-downcase package) }.html"))
    (html5:div `(:class "res" :id ,name)
      (html5:h4 ()
        (html5:a `(:href ,url) package)
        (html5:a `(:href ,#?"${url}#${name}")
          #?" ${name} ${arglist} ")
        #?"<${type}>")
      (html5:div '(:class "description")
        (case type
          (:package
           (or (shorten-string (sr-val d :readme) shorten-to)
               (shorten-string (sr-val d :documentation) shorten-to)))
          (t (shorten-string (sr-val d :documentation) shorten-to)))))))

(defun display-item-fn (d)
  "Display a single item from a package as html"
  (search-result-fn d nil))

(defun package-view (package)
  "Render the view of an entire packages docs"
  (let ((doc (package-document package))
        (items (docs-for-term :package package)))
    (render-window
        (:title #?"Documentation For ${package}") 
      (html5:div '(:class "package")
        (html5:div '(:class "documentation")
          (doc-value doc :documentation))
        (html5:div '(:class "readme")
          (doc-value doc :readme))
        (html5:div '(:class "members")
          (mapcar #'display-item-fn items))))))

(hunchentoot:define-easy-handler (%package-view :uri "/package")
    ((p :parameter-type #'symbol-munger:english->keyword))
  (package-view p))

(defun welcome-view ()
  "Render the home screen of the site"
  (render-window (:title "Common Lisp Documentation Search Engine"
                   :class "welcome")
    (html5:section '()
      (%search-form)
      (html5:p ()
        "Here you can search the doc strings of all packages that are quicklisp loadable. ")
      (html5:p `(:class "about")
        "The source for this website is at "
        (html5:a '(href "https://github.com/AccelerationNet/manifest-search-web")
          "manifest-search-web")
        " with its sister project " 
        (html5:a '(href "https://github.com/AccelerationNet/manifest-search")
          "manifest-search. ")
        "Inspired by and utilizing "
        (html5:a '(href "https://github.com/gigamonkey/manifest")
          "manifest. ")
        (html5:a '(href "https://github.com/gigamonkey/manifest"))
        "This project uses "
        (html5:a '(href "http://code.google.com/p/montezuma/") "montezuma")
        ", a common lisp port of lucene. Searches are performed using a subset of
         lucene syntax. "
        (html5:a '(href "http://l1sp.org")
          "Also see l1sp.org for a different kind of common lisp documentation search."))
      (html5:p '()
        (html5:h4 () "/search")
        "Searches common lisp doc-strings"
        (html5:ul '(:class :parameters)
          (html5:li '(:class "n")
            "The \"n\" parameter controls the number of results (default 50)")
          (html5:li '(:class "q")
            "The \"q\" parameter is the search to be returned")
          (html5:li '(:class "type")
            "The \"type\" parameter is the format of the search
        results.  Defaults to html but also accepts json and lisp.
        Lisp returns a list of plists of search results")))
      (html5:p '()
        (html5:h4 () "/package")
        "Displays the documentation for a given package (and its contents)"
        (html5:ul '(:class :parameters)
          (html5:li '(:class "p")
            "The \"p\" parameter controls the package documentation to display")))
      (html5:p '()
        (html5:h4 () "Document Index")
        "The montezuma index and static package documentation driving this website can be downloaded at: "
        (html5:a '(:href "doc-index.tar.gz" :rel "no-follow")
          "doc-index.tar.gz"))
      (html5:p '(:class "issues")
        (html5:a '(href "https://github.com/AccelerationNet/manifest-search-web")
          "Please report bugs or suggest improvements at the github project page.")))))

(hunchentoot:define-easy-handler (%welcome :uri "/")
    ()
  (welcome-view))