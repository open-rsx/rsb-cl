;;;; util.lisp --- Tests for the utilities used in the model.inference module.
;;;;
;;;; Copyright (C) 2015, 2016, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.model.inference.test)

(test tri-and/smoke
  "Smoke test for the `tri-and' macro."

  (%call-with-tri-test-cases
   (lambda (left left-definitive? right right-definitive?)
     (tri-and (values left left-definitive?)
              (values right right-definitive?)))
   '(((nil nil) (nil nil) (nil nil))
     ((nil nil) (nil t  ) (nil t  ))
     ((nil nil) (t   nil) incompatible-arguments)
     ((nil nil) (t   t  ) (nil nil))
     ((nil t  ) (nil nil) (nil t  ))
     ((nil t  ) (nil t  ) (nil t  ))
     ;; Would be an error but short-circuited.
     ;; ((nil t  ) (t   nil) incompatible-arguments)
     ((nil t  ) (t   t  ) (nil t  ))
     ((t   nil) (nil nil) incompatible-arguments)
     ((t   nil) (nil t  ) incompatible-arguments)
     ((t   nil) (t   nil) incompatible-arguments)
     ((t   nil) (t   t)   incompatible-arguments)
     ((t   t  ) (nil nil) (nil nil))
     ((t   t  ) (nil t  ) (nil t  ))
     ((t   t  ) (t   nil) incompatible-arguments)
     ((t   t  ) (t   t  ) (t   t  )))))

(test tri-or/smoke
  "Smoke test for the `tri-or' macro."

  (%call-with-tri-test-cases
   (lambda (left left-definitive? right right-definitive?)
     (tri-or (values left left-definitive?)
             (values right right-definitive?)))
   '(((nil nil) (nil nil) (nil nil))
     ((nil nil) (nil t  ) (nil nil))
     ((nil nil) (t   nil) incompatible-arguments)
     ((nil nil) (t   t  ) (t   t  ))
     ((nil t  ) (nil nil) (nil nil))
     ((nil t  ) (nil t  ) (nil t  ))
     ((nil t  ) (t   nil) incompatible-arguments)
     ((nil t  ) (t   t  ) (t   t  ))
     ((t   nil) (nil nil) incompatible-arguments)
     ((t   nil) (nil t  ) incompatible-arguments)
     ((t   nil) (t   nil) incompatible-arguments)
     ((t   nil) (t   t)   incompatible-arguments)
     ((t   t  ) (nil nil) (t   t  ))
     ((t   t  ) (nil t  ) (t   t  ))
     ;; Would be an error but short-circuited.
     ;; ((t   t  ) (t   nil) incompatible-arguments)
     ((t   t  ) (t   t  ) (t   t  )))))

;;; Utilities

(defun %call-with-tri-test-cases (function cases)
  (mapc (lambda+ (((left left-definitive?) (right right-definitive?)
                   expected))
          (flet ((do-it ()
                   (multiple-value-list
                    (funcall function
                             left left-definitive?
                             right right-definitive?))))
            (case expected
              (incompatible-arguments
               (signals incompatible-arguments (do-it)))
              (t
               (is (equal expected (do-it)))))))
        cases))
