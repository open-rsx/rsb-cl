;;; rsb.lisp --- Global and special variables used within RSB.
;;
;; Copyright (C) 2010, 2011, 2012 Jan Moringen
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

(cl:in-package :rsb)


;;; Framework timestamps
;;

(declaim (special *framework-timestamps*))

(defvar *framework-timestamps* '(:create :send :receive :deliver)
  "Names of timestamps which are associated to certain points in the
life of RSB events.")


;;; Configuration
;;

(declaim (special *default-configuration-files*))

(defvar *default-configuration-files*
  '(;; Current directory
    "rsb.conf"

    ;; User-specific configuration
    #+(and unix (not darwin)) "~/.config/rsb.conf"
    ;;#+darwin                ?
    #+windows                 "~/rsb.conf"

    ;; System-wide configuration
    #+(and unix (not darwin)) "/etc/rsb.conf"
    #+windows                 "c/rsb.conf"
    ;;#+darwin                ?
    )
  "List of configuration file names in order of decreasing priority.")

(declaim (special *default-configuration*))

(defvar *default-configuration* nil
  "DOC")
