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

(defun fetch (&rest query)
  (setup-auth-if-necessary)
  (setf *last-got*
        (list* (car query)
               (loop
                  for (key value) on (rest query) by #'cddr
                  collect (cons (string-downcase (symbol-name key)) 
                                (format nil "~A" value)))))
  (setf *last-query-url*
        (format nil "http://www.ncdc.noaa.gov/cdo-web/api/v2/~(~A~)" (car *last-got*)))
  (setf *last-query-result*
        (cl-json:decode-json-from-string
         (setf *raw-last-query-result*
               (coerce
                (loop
                   with raw-result-bytes 
                     = (drakma:http-request *last-query-url*
                                            :parameters (rest *last-got*)
                                            :additional-headers *extra-headers*)
                   for byte across raw-result-bytes
                   collect (code-char byte)) 
                'string)))))

