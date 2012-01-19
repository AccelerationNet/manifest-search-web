;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :manifest-search-web.system)
    (defpackage :manifest-search-web.system
	(:use :common-lisp :asdf))))

(in-package manifest-search-web.system)

(defsystem :manifest-search-web
  :description "index and search common lisp documentation using montezuma and manifest"
  :licence "BSD"
  :version "0.1"
  :components ((:file "manifest-search-web"))
  :depends-on (:alexandria
               :collectors :symbol-munger
               :swank ;; for introspection facilities
               :iterate :manifest-search :buildnode-xhtml :cl-interpol
               :hunchentoot :cl-json))

;; (defsystem :manifest-search-web-test
;;   :description "a test for manifest-search-web"
;;   :licence "BSD"
;;   :version "0.1"
;;   :components ((:module :tests
;; 			:serial t
;; 			:components ((:file "manifest-search-web"))))
;;   :depends-on (:manifest-search-web :lisp-unit))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :manifest-search-web))))
  (asdf:oos 'asdf:load-op :manifest-search-web-test))

;; Copyright (c) 2011 Russ Tyndall , Acceleration.net http://www.acceleration.net
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
