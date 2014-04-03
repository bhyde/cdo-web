;-*- mode:common-lisp -*-

(defsystem "cdo-web-examples"
  :depends-on ("cdo-web"
               "fare-quasiquote-readtable" 
               "optima.ppcre"
               "zipcode-distance-api")
  :components ((:file "examples.lisp")))

