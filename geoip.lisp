;;;; +----------------------------------------------------------------+
;;;; | GeoIP                                                          |
;;;; +----------------------------------------------------------------+

(defpackage #:geoip/geoip
  (:use #:cl)
  (:import-from
   #:asdf
   #:system-relative-pathname)
  (:import-from
   #:split-sequence
   #:split-sequence-if
   #:split-sequence)
  (:import-from
   #:fare-csv
   #:read-csv-line)
  (:import-from
   #:constantia
   #:lower-bound
   #:binary-search)
  (:export
   #:geoip-database
   #:geoip-lookup-country-ipv4
   #:*geoip-database*
   #:geoip
   #:create-geolite2-database))

(in-package #:geoip/geoip)

(defclass geoip-database ()
  ())

(defgeneric geoip-lookup-country-ipv4 (geoip-database ipv4-address))

(defvar *geoip-database* nil)

(defun geoip (ipv4-address-string &key (database *geoip-database*))
  (when (and (null database)
             (null *geoip-database*))
    (setf *geoip-database* (create-geolite2-database)))
  (geoip-lookup-country-ipv4 *geoip-database* (decode-ipv4-address ipv4-address-string)))

;; Utilities

(defun decode-ipv4-address (string)
  (let ((tokens (split-sequence #\. string)))
    (assert (= 4 (length tokens)))
    (destructuring-bind (a b c d)
        (mapcar #'parse-integer tokens)
      (logior (ash a 24) (ash b 16) (ash c 8) d))))

(defun decode-ipv4-address-range (string)
  (let ((tokens (split-sequence-if (lambda (char) (find char "./")) string)))
    (assert (= 5 (length tokens)))
    (destructuring-bind (a b c d mask)
        (mapcar #'parse-integer tokens)
      (let* ((start (logior (ash a 24) (ash b 16) (ash c 8) d))
             (mask-bits (1- (ash 1 (- 32 mask))))
             (end (logior (logandc2 start mask-bits) mask-bits)))
        (list start end)))))

(defun keywordize (string)
  (intern (string-upcase string)
          (load-time-value (find-package "KEYWORD"))))

;; GeoLite2

(defstruct (geolite2-country-block-ipv4
            (:conc-name cb-))
  network
  geoname-id
  registered-country-geoname-id
  represented-country-geoname-id
  is-anonymous-proxy
  is-satellite-provider
  ;; Computed slots
  decoded-network)

(defun read-geolite2-country-blocks-ipv4-csv (filename)
  (with-open-file (stream filename :direction :input)
    (read-csv-line stream)
    (coerce
     (loop for fields = (read-csv-line stream)
           while fields
           when (= (length fields) 6)
           collect (destructuring-bind (network
                                        geoname-id
                                        registered-country-geoname-id
                                        represented-country-geoname-id
                                        is-anonymous-proxy
                                        is-satellite-provider)
                       fields
                     (make-geolite2-country-block-ipv4
                      :network network
                      :geoname-id (ignore-errors (parse-integer geoname-id))
                      :registered-country-geoname-id (ignore-errors (parse-integer registered-country-geoname-id))
                      :represented-country-geoname-id (ignore-errors (parse-integer represented-country-geoname-id))
                      :is-anonymous-proxy (not (equal is-anonymous-proxy "0"))
                      :is-satellite-provider (not (equal is-satellite-provider "0"))
                      :decoded-network (decode-ipv4-address-range network))))
     'vector)))

(defstruct (geolite2-country-location
            (:conc-name cl-))
  geoname-id
  locale-code
  continent-code
  continent-name
  country-iso-code
  country-name
  is-in-european-union)

(defun read-geolite2-country-locations-csv (filename)
  (with-open-file (stream filename :direction :input)
    (read-csv-line stream)
    (coerce
     (loop for fields = (read-csv-line stream)
           while fields
           when (= (length fields) 7)
           collect (destructuring-bind (geoname-id
                                        locale-code
                                        continent-code
                                        continent-name
                                        country-iso-code
                                        country-name
                                        is-in-european-union)
                       fields
                     (make-geolite2-country-location
                      :geoname-id (ignore-errors (parse-integer geoname-id))
                      :locale-code locale-code
                      :continent-code continent-code
                      :continent-name continent-name
                      :country-iso-code (keywordize country-iso-code)
                      :country-name country-name
                      :is-in-european-union (not (equal is-in-european-union "0")))))
     'vector)))

(defclass geolite2-database (geoip-database)
  ((country-blocks-ipv4
    :initarg :country-blocks-ipv4
    :reader geolite2-database-country-blocks-ipv4)
   (country-locations
    :initarg :country-locations
    :reader geolite2-database-country-locations)))

(defun create-geolite2-database (&optional directory)
  (let ((directory (or directory (system-relative-pathname "geoip" "GeoLite2-Country-CSV_20200303/"))))
    (make-instance 'geolite2-database
                   :country-blocks-ipv4 (read-geolite2-country-blocks-ipv4-csv
                                         (merge-pathnames
                                          (make-pathname :name "GeoLite2-Country-Blocks-IPv4" :type "csv")
                                          directory))
                   :country-locations (read-geolite2-country-locations-csv
                                       (merge-pathnames
                                        (make-pathname :name "GeoLite2-Country-Locations-en" :type "csv")
                                        directory)))))

(defmethod cb-network-start ((cb geolite2-country-block-ipv4))
  (first (cb-decoded-network cb)))

(defmethod cb-network-end ((cb geolite2-country-block-ipv4))
  (second (cb-decoded-network cb)))

(defmethod geoip-lookup-country-ipv4 ((database geolite2-database) ipv4-address)
  (let* ((the-cb nil)
         (the-cl nil)
         (the-country-iso-code nil)
         (the-country-name nil)
         (country-blocks (geolite2-database-country-blocks-ipv4 database))
         (cb-index (lower-bound ipv4-address country-blocks :key #'cb-network-end)))
    (when (< cb-index (length country-blocks))
      (let ((cb (aref country-blocks cb-index)))
        (when (>= ipv4-address (cb-network-start cb))
          (setf the-cb cb)
          (let ((country-id (or (cb-represented-country-geoname-id the-cb)
                                (cb-registered-country-geoname-id the-cb))))
            (when country-id
              (let ((country-locations (geolite2-database-country-locations database)))
                (setf the-cl (binary-search country-id country-locations :key #'cl-geoname-id))
                (when the-cl
                  (setf the-country-iso-code (cl-country-iso-code the-cl))
                  (setf the-country-name (cl-country-name the-cl)))))))))
    (values the-country-iso-code the-country-name the-cb the-cl)))
