;-*- mode:common-lisp -*-

(defsystem "cdo-web-examples"
  :depends-on ("cdo-web"
               "fare-quasiquote-readtable" 
               "fare-quasiquote-optima"
               "optima.ppcre"
               "zipcode-distance-api")
  :components ((:file "examples")))

