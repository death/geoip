;;;; +----------------------------------------------------------------+
;;;; | GeoIP                                                          |
;;;; +----------------------------------------------------------------+

(asdf:defsystem #:geoip
  :description "Return geographical information associated with an IP address."
  :author "death <github.com/death>"
  :license "MIT"
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("geoip/all"))
