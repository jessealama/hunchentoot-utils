;;; packages.lisp Definition of our package(s)

(in-package :cl-user)

(defpackage :hunchentoot-utils
  (:use :cl :hunchentoot :cl-who)
  (:export "WITH-XML-DECLARATION"
	   "WITH-HTML"
	   "WITH-TITLE"
	   "DEFINE-XML-HANDLER"
	   "DEFINE-XHTML-HANDLER"
	   "CREATE-STATIC-PAGE-DISPATCHER"
	   "FETCH-POST-PARAMETERS"))

;;; packages.lisp ends here