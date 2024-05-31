(in-package :cl-user)
(defpackage :pre-programming-exercise
  (:use :cl
   :alexandria)
  (:export :places-alist
   :places-hash-table
           :location-names
   :place-by-location-name
           :weather-api-url
   :place-location-report))
(in-package :pre-programming-exercise)

(defun places-alist (file)
  (with-open-file (in file)
    (read in)))

(defun places-hash-table (file)
  (alist-hash-table (places-alist file)
                    :test #'equal))

(defgeneric location-names (collection))

(defmethod location-names ((collection list))
  (mapcar #'car collection))

(defmethod location-names ((collection hash-table))
  (hash-table-keys collection))

(defgeneric place-by-location-name (name collection))

(defmethod place-by-location-name ((name string) (collection list))
  (cdr (assoc name collection :test #'string-equal)))

(defmethod place-by-location-name ((name string) (collection hash-table))
  (gethash name collection))

(defun weather-api-url (place)
  (format nil "https://api.weather.gov/points/~0,2F,~0,2F"
          (getf place :latitude)
          (getf place :longitude)))

(defun place-location-report (collection)
  (dolist (location (location-names collection))
    (let ((place (place-by-location-name location collection)))
      (format t "~A is located at ~0,2F~C ~0,2F~C, ~A~%"
              (getf place :interesting-place)
              (getf place :latitude)
              (if (plusp (getf place :latitude)) #\N #\S)
              (getf place :longitude)
              (if (plusp (getf place :longitude)) #\E #\W)
              location))))

