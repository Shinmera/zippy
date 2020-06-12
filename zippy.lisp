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

(defgeneric decompress-vector (vector index length format))
(defgeneric decompress-stream (stream length format))
(defgeneric compress-vector (vector out-stream format))
(defgeneric compress-stream (in-stream out-stream format))

(defmethod decompress-vector (vector index length (format (eql :deflate)))
  (3bz:decompress-vector vector :format :deflate :start index :end (+ index length)))
