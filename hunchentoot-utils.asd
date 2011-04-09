
(in-package :cl-user)

(defpackage :hunchentoot-utils-asd
  (:use :cl :asdf))

(in-package :hunchentoot-utils-asd)

(defsystem :hunchentoot-utils
  :description "Some utilities for defining dynamic web sites with hunchentoot"
  :long-description 
"The hunchentoot web server (http://www.weitz.de/hunchentoot/) is a
powerful dynamic foundation on which to build high-quality (and fast?)
web sites. This package contains some utilities that I wrote when
writing my first hunchentoot application.  While working on further
hunchentoot web sites, I found myself copying previousy written code,
which caused great mental anguish (I guess I was raised right\) So I
thought it would make sense to separate these utilities from my
specific application so that I could have a share them among my
hunchentoot-based projects.  Maybe some day others who are interested
in writing web applications using lisp and hunchentoot will find these
useful, too.

FYI: although the aim is to provide utilities for hunchentoot, and I
call this thing \"hunchentoot-utils\", there are other players afoot.
How can I write utilities for hunchentoot without also taking a stand
on the issue of which (X)HTML generation library should be chosen?
Can I really stay neutral on this matter, now that I'm in the thick of
making hunchentoot web sites?  When I got started learning
hunchentoot, I simultaneously learned CL-WHO (by Edi Weitz, the author
of hunchentoot).  So my code is currently intermingled with CL-WHO
code; this system does not depend solely on hunchentoot.  The value of
not depending on CL-WHO is clear to me, but I don't know how to do
that: I don't know how to totally factor out the (X)HTML generation
library from my hunchentoot utilities.  I hope you understand.
Perhaps one day I will reach a sufficiently advanced state of
enlightenment and a nice set of purely hunchentoot utilities will
emerge from this work that would permit one to use whatever (X)HTML
generation library you like.  But for now, \"hunchentoot utils\"
really means: \"hunchentoot (and CL-WHO) utils\".  Sorry."
  :depends-on (:hunchentoot :cl-who :alexandria)
  :author "Jesse Alama <jesse.alama@gmail.com>"
  :version "0"
  :serial t
  :components ((:file "packages")
	       (:file "utils")))
