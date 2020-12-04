;;;; +----------------------------------------------------------------+
;;;; | GeoIP                                                          |
;;;; +----------------------------------------------------------------+

(defpackage #:geoip/protocol
  (:use #:cl)
  (:export
   #:geoip-database
   #:geoip-lookup-country-ipv4))

(in-package #:geoip/protocol)

;;;; Protocol

(defclass geoip-database ()
  ())

(defgeneric geoip-lookup-country-ipv4 (geoip-database ipv4-address))
