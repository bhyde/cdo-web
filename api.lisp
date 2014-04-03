(in-package #:common-lisp-user)

(defpackage #:cdo-web 
  (:use #:common-lisp)
  (:export #:fetch))

(in-package #:cdo-web)

;;;; Auth is done with a header.

(defvar *extra-headers* nil)

(defun setup-auth-if-necessary ()
  (unless *extra-headers*
    (unless (get :cdo-auth-info :token)
      (when (probe-file (asdf:system-relative-pathname "cdo-web" "auth.lisp"))
        (load (asdf:system-relative-pathname "cdo-web" "auth.lisp"))))
    (unless (get :cdo-auth-info :token)
      (error "Authentication token not found."))
    (setf *extra-headers* `(("token" . ,(get :cdo-auth-info :token)))))
  nil)

;;;; Fetch.

(defvar *last-query-url*)
(defvar *last-got*)
(defvar *last-query-result*)
(defvar *raw-last-query-result*)

(defun fetch (domain &rest query-plist)
  (setup-auth-if-necessary)
  (setf *last-got*p
        (list* domain
               (loop
                  for (key value) on query-plist by #'cddr
                  collect (cons (string-downcase (symbol-name key)) 
                                (format nil "~A" value)))))
  (setf *last-query-url*
        (format nil "http://www.ncdc.noaa.gov/cdo-web/api/v2/~(~A~)" domain))
  (setf *last-query-result*
        (cl-json:decode-json-from-string
         (let ((drakma:*text-content-types* '(("application" . "json"))))
           (drakma:http-request *last-query-url*
                                :parameters (rest *last-got*)
                                :additional-headers *extra-headers*)))))

