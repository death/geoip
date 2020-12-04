;;;; +----------------------------------------------------------------+
;;;; | GeoIP                                                          |
;;;; +----------------------------------------------------------------+

(defpackage #:geoip/util
  (:use #:cl)
  (:import-from
   #:split-sequence
   #:split-sequence-if
   #:split-sequence)
  (:export
   #:decode-ipv4-address
   #:decode-ipv4-address-range
   #:keywordize))

(in-package #:geoip/util)

;;;; Utilities

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
