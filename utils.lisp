;;; utils.lisp A collection of utilities for the hunchentoot web server

(in-package :hunchentoot-utils)

(defmacro with-xml-declaration (&body body)
  `(with-html-output-to-string (s)
     "<?xml version='1.1' encoding='UTF-8'?>"
     ,*prologue*
     (htm ,@body)))

(defmacro with-html (&body body)
  `(with-xml-declaration
     (:html :xmlns "http://www.w3.org/1999/xhtml"
	,@body)))

(defmacro with-title (title &body body)
  `(with-html
     (:head (:title ,title))
     (:body ,@body)))

(defmacro define-xml-handler (name (&rest args) &body body)
  `(defun ,name (,@args)
     (setf (content-type*) "application/xhtml+xml")
     ,@body))

(defmacro define-xhtml-handler (name (&rest args) &body body)
  `(defun ,name (,@args)
     (setf (content-type*) "text/html")
     ,@body))


;;; utils.lisp ends here