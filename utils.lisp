;;; utils.lisp A collection of utilities for the hunchentoot web server

(in-package :hunchentoot-utils)

(defun pairs (list)
  (labels ((pairs-int (lst)
	     (when lst
	       (if (cdr lst)
		   (cons (list (first lst) (second lst))
			 (pairs-int (cddr lst)))
		   (error "The list ~a does not have even length" list)))))
    (pairs-int list)))

(defparameter *output-mode* 'html
  "Whether we serve our representations as XML or HTML.")

(defparameter *xml-version* "1.0"
  "The version of XML that we output.  Acceptable values are the
  strings \"1.0\" and \"1.1\".")

(defparameter *xhtml-version* "1.1"
  "The vesion of XHTML to which we declare we adhere.  Permissible values are the strings \"1.1\" and \"1.0\".")

(defun output-mime-type (output-mode-symbol)
  (if (eq output-mode-symbol 'xml)
      "application/xhtml+xml; charset=UTF-8"
      "text/html; charset=UTF-8"))

(defun xml-declaration (maybe-xml-version-string)
  (eswitch (maybe-xml-version-string :test #'string=)
    ("1.0" "<?xml version='1.0' encoding='UTF-8'?>")
    ("1.1" "<?xml version='1.1' encoding='UTF-8'?>")))

(defun doctype (maybe-xhtml-version-string)
  (eswitch (maybe-xhtml-version-string :test #'string=)
    ("1.1" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
    ("1.0" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")))

(defmacro with-xml-declaration
    ((&rest rest
      &key (content-type (output-mime-type *output-mode*))
           (xml-declaration (xml-declaration *xml-version*))
	   (doctype (doctype *xhtml-version*))
	   (return-code +http-ok+)
	   &allow-other-keys)
     &body body)
  (let* ((pairs (pairs rest))
	 (bad-pairs (remove-if #'keywordp pairs :key #'first))
	 (non-header-keywords '(:content-type
				:xml-declaration
				:doctype
				:return-code)))
    (flet
	((headerp-in-rest (x) (member x non-header-keywords :test #'eq)))
      (if bad-pairs
	  (error "The arguments ~a are unacceptable because we expected to find a keyword in every odd position, but '~a' is not a keyword" rest (caar bad-pairs))
	  (let ((only-headers (remove-if #'headerp-in-rest pairs :key #'first)))
	    (loop
	       for (param value) in only-headers
	       collect (list 'setf (list 'header-out param) value) into headers
	       finally
		 (return
		   `(progn
		      ,@headers
		      (setf (return-code *reply*) ,return-code)
		      (with-html-output-to-string (s nil :indent nil)
			(str ,xml-declaration)
			(str ,doctype)
			(htm ,@body))))))))))

(defmacro with-html ((&rest rest
		      &key (content-type "text/html; charset=UTF-8")
		           (xml-declaration "<?xml version='1.0' encoding='UTF-8'?>")
			   (doctype "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
			   (return-code +http-ok+)
		       &allow-other-keys)
		      &body body)
  `(with-xml-declaration (:content-type ,content-type
			  :xml-declaration ,xml-declaration
			  :doctype ,doctype
			  :return-code ,return-code
			  ,@rest)
     ((:html :xmlns "http://www.w3.org/1999/xhtml")
       ,@body)))

(defmacro with-html5 ((&rest rest
		       &key (content-type "text/html; charset=UTF-8")
		            (xml-declaration "<?xml version='1.0' encoding='UTF-8' standalone='yes'?>")
			    (doctype "<!DOCTYPE html>")
			    (return-code +http-ok+)
			    (lang "en")
		       &allow-other-keys)
		      &body body)
  `(with-xml-declaration (:content-type ,content-type
			  :xml-declaration ,xml-declaration
			  :doctype ,doctype
			  :return-code ,return-code
			  ,@rest)
     ((:html :lang ,lang)
       ,@body)))

(defmacro with-title ((title)
		      (&rest rest
		       &key (content-type "text/html; charset=UTF-8")
		            (xml-declaration "<?xml version='1.0' encoding='UTF-8'?>")
			    (doctype "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
			    (return-code +http-ok+)
			    &allow-other-keys)
		      &body body)
  (let ((emitted-title (cond ((stringp title) title)
			     ((symbolp title) (format nil "(str ~a)" title))
			     (t (error "Unsure how to handle the title '~a'" title)))))
    `(with-html (:content-type ,content-type
	         :xml-declaration ,xml-declaration
		 :doctype ,doctype
		 :return-code ,return-code
		 ,@rest)
       (:head (:title ,emitted-title))
       (:body ,@body))))

(defmacro with-favicon-and-title (favicon-url title &body body)
  `(with-html
     (:head
      (:link :rel "icon" :href ,favicon-url :type "image/x-icon")
      (:title ,title))
     (:body ,@body)))

(defmacro define-xml-handler (name (&rest args) &body body)
  `(defun ,name (,@args)
     (setf (content-type*) "application/xhtml+xml")
     ,@body))

(defmacro define-xhtml-handler (name (&rest args) &body body)
  `(defun ,name (,@args)
     (setf (content-type*) "text/html")
     ,@body))

(defmacro create-static-page-dispatcher (page handler)
  "Create a hunchentoot dispatcher that dispatches to HANDLER only for
exact matches for PAGE.  PAGE is a string that begins with \"/\".
Other than that, it should contain only alphanumeric characters. (In
particular, it should not contain a dollar symbol \"$\"). HANDLER is a
symbol naming a function that can handle requests that match
page (exactly).

This macro was created to fill a gap that exists in hunchentoot.  One
can create dispatchers that dispatch based on requests that match a
*prefix* of a string, and one can create dispatchers that dispatch
based on requests matching a *regular expression*.  I wanted a clean
way to create dispatchers that dispatch based on *exact* matches.
Thus, I want to dispatch requests for, say, \"/foo\" to go to
FOO-HANDLER.  If the client requests \"/foobar\", I want it to go to
some other handler (or generate a response with code 404).

Obviously, this kind of thing doesn't really arise in file-based HTTP
servers.  If there's a file called \"foo\" under the root document
directory, then evidently it is the thing to be served when one
requests \"/foo\".  If one requests \"/foobar\" and there is no such
file with that name under the root document directory, then the
response of the HTTP server will indicate an error.  But in the more
dynamic, not-necessarily-file-based approach taken by hunchentoot, one
has the flexibility to dispatch in all sorts of ways depending on the
requested resource; the space of resources that could be successfully
served is limited only by what one can compute based on the request;
the set of requestable resources isn't limited to what files exist
under the root document directory.

Using regular expressions, à la CL-PPCRE, is obviously the way to
solve the problem.  Given a static \"page\" name like \"index\", we
create a regular expression that matches only the string \"index\".
Hunchentoot can do this (with a little help from CL-PPCRE) But I
didn't want to pollute my application code with regular expressions,
which are just a means to solve the problem; there's nothing
interesting about them in this context.  Regular expressions are just
a way to address a gap in hunchentoot.

I don't know whether this is going to be an efficient solution.  I may
need to design another solution later, if the need arises."
  `(create-regex-dispatcher (concatenate 'string
					 "^"
					  ,page
					  "$")
			     ,handler))

(defun fetch-post-parameters (&rest params)
  (apply #'values
	 (mapcar #'post-parameter params)))

(defmacro with-valid-session ((&key (title "Invalid session")
				    (return-code 409)
				    (explanation-forms nil)) &body body)
  `(cond ((session-verify *request*) ,@body)
	 (t
	  (setf (return-code*) ,return-code)
	  (with-title ,title
	    (quote ,explanation-forms)))))

;;; utils.lisp ends here