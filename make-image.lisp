;;;; -*- lisp -*-
(in-package :cl-user)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; This script creates, a lisp image which already contains Manifest-Search-Web.
;;;; It assumes ADWCodeBase/misc/sbcl-publish-init.lisp has been run
;;;; as the sbcl init file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-user::setup-source-registry))

(ql:quickload :manifest-search-web)
(defpackage :manifest-search-web-binary
  (:use :cl :cl-user :manifest-search :manifest-search-web))

(in-package :manifest-search-web-binary)
(cl-interpol:enable-interpol-syntax)

(manifest-search:close-index)

(defun process-arg (arg)
  "Removes quotes if they exist"
  (let ((last (- (length arg) 1)))
    (if (and (char-equal (elt arg 0) #\")
	     (char-equal (elt arg last) #\"))
	(subseq arg 1 last)
	arg
	)))
(defun process-args (args)  (mapcar #'process-arg args))
(defvar *args* (process-args sb-ext:*posix-argv*)
  "The command line arguments")
(defun has-arg (name)  (member name *args* :test #'string-equal))
(defun arg-value (name)  (cadr (has-arg name)))

(defparameter +port+ (or (ignore-errors (parse-integer (arg-value "--port")))
                   8888))

(defparameter +web-root+
      (or (arg-value "--wwwroot")
          (arg-value "--webroot")))

(defun main ()
  (sb-posix:setuid (sb-posix:passwd-uid (sb-posix:getpwnam "www-data")))
  (sb-posix:setgid (sb-posix:group-gid (sb-posix:getgrnam "www-data")))
  (setf *web-root*
        (or +web-root+
            ;; just set this explicitly for now
            (truename "/var/www/lisp-search/lisp-search.acceleration.net/current/")
            ;; need the trailing slash to be there
            ;;(pathname #?"${(sb-posix:getcwd)}/../")
            ))
  (setf manifest-search:+index-path+ (resource-path "doc-index"))
  (manifest-search:load-index)
  (manifest-search-web:start-server +port+)
  (push
   (lambda () (manifest-search-web:stop-server))
   sb-ext::*exit-hooks*)
  (sleep most-positive-fixnum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Save the image
(let* ((binary (arg-value "--binary"))
       (binpath (merge-pathnames #?"bin/${ binary }"
                                 *load-truename*)))
  (sb-ext:save-lisp-and-die
   binpath
   :toplevel #'main
   :executable t))

