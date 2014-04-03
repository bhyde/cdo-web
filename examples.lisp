(defpackage #:cbo-web-examples
  (:use #:common-lisp #:cbo-web
        #:optima #:optima.ppcre #:optima.extra
        #:editor-hints.named-readtables
        #:zipcode-distance-api))

(in-package #:cbo-web-examples)

(in-readtable :fare-quasiquote)


(defun stations-near-zipcode (zipcode 
                              &key
                                (types '(:emnt :emxt))
                                (width 2))
                              
  (ematch (zipcode-info zipcode)
    ((alist (:lat . lat) (:lng . lng))
     (let* ((w (/ width 2))
            (extent (format nil "~G,~G,~G,~G" (- lat w)(- lng w)(+ lat w)(+ lng w)))
            (type (loop for k in types nconc `(:datatypeid ,k))))
       (ematch (apply #'fetch :stations :extent extent types)
           ((alist (:results . stations))
            (loop for s in stations
               when (match s ((alist (:maxdate . (ppcre "^2014"))) t)) collect s)))))))

(defun station-nearest-zipcode (zipcode)
  (ematch (zipcode-info zipcode)
    ((alist (:lat . lat) (:lng . lng))
     (let* ((w 1))
       (ematch (fetch `(stations
                        ("limit" . "500")
                        ("datatypeid" . "EMNT")
                        ("datatypeid" . "EMXT")
                        ("extent"
                         . ,(format nil "~G,~G,~G,~G" (- lat w)(- lng w)(+ lat w)(+ lng w)))))
         ((alist (:results . stations))
          (flet ((dis (x y)
                   (let ((dx (- lat x))
                         (dy (- lng y)))
                     (+ (* dx dx) (* dy dy)))))
            (print (length (setf zzz stations)))
            (loop
               with sdist
               with min-dist = 10000
               with min-station = nil
               for station in stations
               do (match station
                    ((alist (:maxdate . (ppcre "^2014-03"))
                            (:latitude . slat)
                            (:longitude . slon))
                     (setf sdist (dis slat slon))
                     (when (< sdist min-dist)
                       (setf min-station station
                             min-dist sdist))))
               finally (return (and min-station
                                    (values min-station (sqrt min-dist))))))))))))
