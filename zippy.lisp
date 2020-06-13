#|
 This file is a part of zippy
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.zippy)

(defun handle-file-header (record)
  (let* ((version (local-file-header-version record))
         (file-attribute-compatibility (gethash (ldb (byte 8 8) version) *file-attribute-compatibility-map*)))
    (multiple-value-bind (major minor) (floor version 10)
      
      (when (logbitp 2 (local-file-header-flags record))
        ;; FIXME: a header may be here.
        (read-data-descriptor stream)))))

(defclass zip-file ()
  ())

(defclass zip-entry ()
  (attribute-compatibility
   version
   encryption-method
   compression-method
   last-modified
   crc-32
   start
   size
   uncompressed-size
   file-name
   extra-fields))
