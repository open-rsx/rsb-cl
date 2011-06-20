;;; clon.lisp --- Commandline option definitions for cl-rsb.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rsb)

(defun make-options ()
  "Make and return commandline option definitions. The returned option
tree has a `com.dvlsoft.clon:group' instance at its root."
  (com.dvlsoft.clon:defgroup (:header "RSB Options")

    (group (:header "Plugins")
	   (path    :long-name     "rsb-plugins-path"
		    :type          :directory-list
		    :default-value nil
		    :env-var       "RSB_PLUGINS_LISP_PATH"
		    :description
		    "A list of directories from which RSB plugins should be loaded. Plugins are ASDF-systems residing in immediate sub-directories of DIRECTORIES. For example: when an item /plugins/ is present, a plugin /plugins/my-plugin/my-plugin.asd would be located successfully.")
	   (lispobj :long-name     "rsb-plugins-load"
		    :typespec      'list
		    :env-var       "RSB_PLUGINS_LISP_LOAD"
		    :argument-name "NAMES"
		    :description
		    "A list of names of ASDF-systems that provide RSB plugins. Each system has to be loadable via ASDF after processing the plugin search path."))

    (group (:header "Quality of Service")
	   (enum    :long-name     "rsb-qualityofservice-reliability"
		    :enum          '(:reliable :unreliable)
		    :default-value :reliable
		    :env-var       "RSB_QUALITYOFSERVICE_RELIABILITY"
		    :argument-name "RELIABILITY"
		    :description   "This option is used to request the desired reliability of network transports and event processing. Requesting better reliability may harm performance.")
	   (enum    :long-name     "rsb-qualityofservice-ordering"
		    :enum          '(:ordered :unordered)
		    :default-value :ordered
		    :env-var       "RSB_QUALITYOFSERVICE_ORDERING"
		    :argument-name "ORDERING"
		    :description   "This option is used to request ordering guarantees regarding network transports and event processing. Requesting stricter ordering guarantees may harm performance."))

    (group (:header "Error Handling")
	   (enum    :long-name     "rsb-errorhandling-onhandlererror"
		    :enum          '(:log :print :exit)
		    :default-value :log
		    :env-var       "RSB_ERRORHANDLING_ONHANDLERERROR"
		    :argument-name "BEHAVIOR"
		    :description   "This option selects a policy for dealing with errors that occur during event handling in client code."))

    ;; Connector options
    (make-options-for-connector-classes)))


;;; Option-level functions
;;

(defun commandline-option-name (&rest components)
  "Return a commandline option name based on COMPONENTS."
  (format nil "~(~{~A~^-~}~)" components))

(defun environment-variable-name (&rest components)
  "Return an environment variable name based on COMPONENTS."
  (format nil "~@:(~{~A~^_~}~)" components))

(defun option-type->class-and-args (type)
  "Return a suitable option class constructor for TYPE and potentially
initargs as multiple values."
  (cond
    ((and (listp type)
	  (eq (first type) 'member)
	  (every #'symbolp (rest type)))
     (values
      #'com.dvlsoft.clon:make-enum
      :enum (rest type)))
    ((subtypep type 'boolean)
     #'com.dvlsoft.clon:make-switch)
    ((subtypep type 'string)
     #'com.dvlsoft.clon:make-stropt)
    ((subtypep type 'integer)
     (values
      #'com.dvlsoft.clon:make-lispobj
      :typespec type))
    (t
     (values
      #'com.dvlsoft.clon:make-lispobj
      :typespec type))))

(defun make-connector-option (class name type
			      &key
			      (default nil default-supplied?)
			      description)
  "Make and return an option object based on the data CLASS, NAME,
TYPE, DEFAULT and description. The kind of commandline option is
determined based on TYPE by calling `option-type->class-and-args'."
  (bind (((constructor &rest args)
	  (multiple-value-list (option-type->class-and-args type))))
    (apply constructor
	   :long-name (commandline-option-name
		       :rsb :transport class name)
	   :env-var   (environment-variable-name
		       :rsb :transport class name)
	   (append
	    (when default-supplied?
	      (list :default-value (eval default))) ;; TODO do the eval earlier
	    (when description
	      (list :description description))
	    args))))

(declaim (special *emitted-options*))

(defvar *emitted-options* (make-hash-table :test #'equal)
  "This variable is used to keep track of already emitted option to
avoid duplicate emissions. The variable should be let-bound to a
hash-table with `equal' test.")

(defun maybe-make-connector-option (class name
				    &rest args)
  "Return a commandline option object for the connector CLASS
according to NAME and ARGS. If the option object has already been
created, the existing object is returned."
  (let ((key (cons class name)))
    (if (gethash key *emitted-options*)
	(list :repeated (commandline-option-name
			 :rsb :transport class name))
	(setf (gethash key *emitted-options*)
	      (apply #'make-connector-option class name args)))))

(defun make-connector-options (name class)
  "Return a list of commandline option objects for the connector class
CLASS. NAME is used when referring to the connector in generated
strings."
  (iter (for option in (cons
			(list :enabled 'boolean
			      :default nil
			      :description
			      "Controls whether this transport should be part of the set of transports that are used by default, that is when no transport is specified explicitly.")
			(rsb.transport:connector-options class)))
	(let ((item (apply #'maybe-make-connector-option name option)))
	  (if (listp item)
	      (collect (second item) :into repeated)
	      (appending (list :item item) :into items)))
	(finally (return (values items repeated)))))


;;; Connector-level functions
;;

(defparameter +no-options-text+
  (com.dvlsoft.clon:make-text
   :contents "This connector does not have any configuration options."))

(defun make-schemas-text (schemas)
  "Return a `clon:text' instance that describes the list of supported
schemas SCHEMAS."
  (com.dvlsoft.clon:make-text
   :contents (format nil "This connector ~:[does not support any ~
schemas~;supports the following schema~P: ~2:*~{~(~A~)~^, ~}~]."
		     schemas (length schemas))))

(defun make-repeated-text (names)
  "Make and return a `clon:text' instance that lists options which
have been excluded as a result of repeated occurrence."
  (com.dvlsoft.clon:make-text
   :contents (format nil "This connector also supports the following ~
option~P which ~:[have~;has~] been described above: ~{~(~A~)~^, ~}."
		     (length names) (length= 1 names) names)))

(defun make-options-for-connector-class (class)
  "Return a `clon:group' instances which contains configuration
options for connector class CLASS."
  (bind (((:accessors-r/o
	   (schemas   rsb.transport:connector-schemas)
	   (direction rsb.transport:connector-direction)) class)
	 (name        (first schemas))
	 (pretty-name (format nil "~:(~A~) ~:(~A~) Connector"
			      name direction))
	 ((:values items repeated)
	  (make-connector-options name class)))
    (apply #'com.dvlsoft.clon:make-group
	   :header pretty-name
	   :item   (make-schemas-text schemas)
	   (append
	    (or items (unless repeated
			(list :item +no-options-text+)))
	    (when repeated
	      (list :item (make-repeated-text repeated)))))))

(defun make-options-for-connector-classes ()
  (let ((*emitted-options* (make-hash-table :test #'equal)))
    (apply #'com.dvlsoft.clon:make-group
	   :header "Connector Options"
	   (iter (for (name class) in (rsb.transport:transport-classes))
		 (appending
		  (list :item (make-options-for-connector-class class)))))))
