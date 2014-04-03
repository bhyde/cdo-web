;-*- mode:common-lisp -*-

(defsystem "cdo-web"
  :depends-on ("drakma" "cl-json")
  :components ((:file "api")))

