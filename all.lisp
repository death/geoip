;;;; +----------------------------------------------------------------+
;;;; | GeoIP                                                          |
;;;; +----------------------------------------------------------------+

(uiop:define-package #:geoip/all
  (:nicknames #:geoip)
  (:use-reexport #:geoip/geoip
                 #:geoip/protocol))
