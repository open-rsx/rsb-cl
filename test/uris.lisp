;;; uris.lisp --- Unit tests for URI-related functions.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rsb.test)

(deftestsuite uris-root (root)
  ()
  (:documentation
   "Unit tests for URI-related functions."))

(addtest (uris-root
          :documentation
	  "Smoke test for the `uri-options' function.")
  uri-options-smoke

  (ensure-cases (uri-string expected)
      '((""                      nil)
	("spread:"               nil)
	("?"                     nil)
	("?foo=bar"              (:foo "bar"))
	("?bar=baz;whoop=nooble" (:bar "baz" :whoop "nooble"))
	("?first=yes&amp=yes"    (:first "yes" :amp "yes"))
	("?host=foo;port=4444"   ()))

    (let* ((uri    (puri:parse-uri uri-string))
	   (result (uri-options uri)))
      (ensure-same result expected
		   :test   #'equal
		   :report "~@<Extracted option from URI ~S are ~S, not ~S.~@:>"
		   :arguments (uri result expected)))))

(addtest (uris-root
          :documentation
	  "Smoke test for the `scope->uri-and-options' function.")
  uri->scope-and-options-smoke

  (ensure-cases (uri-string defaults expected-scope expected-options)
      '(;; From https://code.cor-lab.de/projects/rsb/wiki/URI_Schema
	(""                              nil "/"        nil)
	("spread:"                       nil "/"        ((:spread)))
	("inprocess:"                    nil "/"        ((:inprocess)))
	("spread://localhost:5555"       nil "/"        ((:spread :host "localhost" :port 5555)))
	("inprocess://someotherhost"     nil "/"        ((:inprocess :host "someotherhost")))
	("spread:/foo/bar"               nil "/foo/bar" ((:spread)))
	("spread:?maxfragmentsize=10000" nil "/"        ((:spread :maxfragmentsize "10000")))
	;; Additional
	("foo:/bla?bar=baz;awesome=no"   nil "/bla"     ((:foo :bar "baz" :awesome "no")))
	("bar:" ((:bar :whoop 10)) "/" ((:bar :whoop 10)))
	("bar://baz:20" ((:bar :port 10)) "/" ((:bar :host "baz" :port 20 :port 10)))
	("bar://baz" ((:bar :port 10)) "/" ((:bar :host "baz" :port 10)))
	("/?foo=5&bar=whoop"
	 ((:spread :port 10 :bar "baz") (:inprocess :bla 5))
	 "/"
	 ((:spread :foo "5" :bar "whoop" :port 10 :bar "baz")
	  (:inprocess :foo "5" :bar "whoop" :bla 5))))

    (let+ ((uri (puri:parse-uri uri-string))
	   ((&values scope options)
	    (uri->scope-and-options uri defaults)))
      (ensure-same scope expected-scope
		   :test      #'scope=
		   :report    "~@<Expected scope ~S, not ~S.~@:>"
		   :arguments (expected-scope scope))
      (ensure-same options expected-options
		   :test      #'equalp
		   :report    "~@<Expected options ~S, not ~S.~@:>"
		   :arguments (expected-options options)))))
