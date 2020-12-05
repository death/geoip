;;;; +----------------------------------------------------------------+
;;;; | GeoIP                                                          |
;;;; +----------------------------------------------------------------+

(defpackage #:geoip/geoip
  (:use #:cl #:geoip/protocol #:geoip/util #:geoip/geolite2)
  (:export
   #:*geoip-database*
   #:geoip))

(in-package #:geoip/geoip)

;;;; Main interface

(defvar *geoip-database* nil)

(defun geoip (ipv4-address-string &key (database *geoip-database*))
  (when (and (null database)
             (null *geoip-database*))
    (setf *geoip-database* (create-geolite2-database))
    (setf database *geoip-database*))
  (geoip-lookup-country-ipv4 database (decode-ipv4-address ipv4-address-string)))
