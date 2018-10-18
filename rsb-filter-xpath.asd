;;;; rsb-xpath-filter.asd --- System containing XPath-based filter.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (find-package '#:rsb-system)
    (load (merge-pathnames "rsb.asd" *load-truename*))
    (values))

(cl:in-package #:rsb-system)

(defsystem "rsb-filter-xpath"
  :description "Filter that uses XPath expressions to discriminate events."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(version/string)
  :depends-on  ("xpath"
                (:version "architecture.builder-protocol.xpath" "0.7")

                (:version "rsb"                                 #.(version/string :revision? t))
                (:version "rsb-builder"                         #.(version/string :revision? t)))

  :components  ((:module     "filter"
                 :pathname   "src/filter"
                 :components ((:file       "xpath-filter"))))

  :in-order-to ((test-op (test-op "rsb-filter-xpath/test"))))

(defsystem "rsb-filter-xpath/test"
  :description "Unit tests for the rsb-transport-spread system."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(version/string)
  :depends-on  ((:version "lift"             "1.7.1")

                (:version "rsb"              #.(version/string))
                (:version "rsb-filter-xpath" #.(version/string))

                (:version "rsb/test"         #.(version/string)))

  :components  ((:module     "filter"
                 :pathname   "test/filter"
                 :components ((:file       "xpath-filter"))))

  :perform     (test-op (operation component)
                 (eval (read-from-string "(log:config :warn)")) ; less noise
                 (eval (read-from-string
                        "(lift:run-tests :config (lift::lift-relative-pathname
                                                  \"lift-filter-xpath.config\"))"))))
