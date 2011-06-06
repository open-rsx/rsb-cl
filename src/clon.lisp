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
		    :description   "This option selects a policy for dealing with errors that occur during event handling in client code."))))
