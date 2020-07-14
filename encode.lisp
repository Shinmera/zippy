#|
 This file is a part of zippy
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.zippy)

(defvar *version*
  '(4 5))

(defvar *compatibility*
  (position #+windows :ntfs
            #+darwin :darwin
            #+(and unix (not darwin)) :unix
            *file-attribute-compatibility-map*))

(defun entry-flags (entry)
  (bitfield (encryption-method entry)
            NIL
            NIL
            (null (size entry))
            NIL
            NIL
            (and (encryption-method entry)
                 (not (eql :pkware (encryption-method entry))))
            NIL NIL NIL NIL T NIL NIL NIL NIL))

(defun entry-to-lf (entry)
  (multiple-value-bind (date time) (encode-msdos-timestamp (last-modified entry))
    (let ((file-name (babel:octets-to-string (file-name entry) :encoding :utf-8))
          (extra #()))
      ;; Append Zip64 size info if we can.
      (when (and (size entry) (<= #xFFFFFFFF (size entry)))
        (setf extra (make-array 20 :element-type '(unsigned-byte 8)))
        (nibbles::ub16set/le extra 0 #x0001)
        (nibbles::ub16set/le extra 2 16)
        (nibbles::ub64set/le extra 4 (uncompressed-size entry))
        (nibbles::ub64set/le extra 12 (size entry)))
      (make-local-file (encode-version *version* *compatibility*)
                       (entry-flags entry) (compression-method entry)
                       date time (or (crc-32 entry) 0)
                       (if (size entry) (cap (size entry) 32))
                       (if (uncompressed-size entry) (cap (uncompressed-size entry) 32))
                       (length file-name) (length extra) file-name extra))))

(defun entry-to-dd (entry)
  (if (< (uncompressed-size entry) #xFFFFFFFF)
      (make-data-descriptor (crc-32 entry) (size entry) (uncompressed-size entry))
      (make-data-descriptor/64 (crc-32 entry) (size entry) (uncompressed-size entry))))

(defun entry-to-cd (entry start)
  (multiple-value-bind (date time) (encode-msdos-timestamp (last-modified entry))
    (let ((file-name (babel:octets-to-string (file-name entry) :encoding :utf-8))
          (comment (if (comment entry)
                       (babel:octets-to-string (comment entry) :encoding :utf-8)
                       #()))
          (extra #()))
      (when (<= #xFFFFFFFF (size entry))
        (setf extra (make-array 20 :element-type '(unsigned-byte 8)))
        (encode-structure (make-zip64-extended-information
                           28 (size entry) (uncompressed-size entry)
                           start 0)
                          extra 0))
      (make-central-directory-entry
       (encode-version *version* *compatibility*)
       (encode-version *version* *compatibility*)
       (entry-flags entry)
       (compression-method entry)
       date time (or (crc-32 entry) 0)
       (if (size entry) (cap (size entry) 32))
       (if (uncompressed-size entry) (cap (uncompressed-size entry) 32))
       (length file-name) (length extra) (length comment)
       0 0 0 (cap start 32)
       file-name extra comment))))

(defun encode-entry-payload (entry output password)
  (with-io (input (content entry))
    (let ((start (index input))
          (written 0)
          (encryption-state (make-encryption-state (encryption-method entry) password))
          (compression-state (make-compression-state (compression-method entry) input)))
      (labels ((write-out (buffer start end)
                 (output output buffer start end)
                 (incf written (- end start)))
               (encrypt (buffer start end)
                 (call-with-encrypted-buffer #'write-out buffer start end encryption-state)))
        (call-with-compressed-buffer #'encrypt input compression-state))
      (setf (size entry) written)
      (setf (uncompressed-size entry) (- (index input) start)))))

(defun encode (zip-file output &key password)
  (let ((starts (make-array (length (entries zip-file))))
        (comment (or (comment zip-file) "")))
    (loop for i from 0
          for entry across (entries zip-file)
          do (setf (aref starts i) (index output))
             (write-structure* (entry-to-lf entry) output)
             (encode-entry-payload entry output password)
             (write-structure* (entry-to-dd entry) output))
    (let ((cd-start (index output)))
      (loop for entry across (entries zip-file)
            for start across starts
            do (write-structure* (entry-to-cd entry start) output))
      (let ((cd-end (index output)))
        (write-structure* (make-end-of-central-directory/64
                           44
                           (encode-version *version* *compatibility*)
                           ;; FIXME: be smarter about noting the min version.
                           (encode-version *version* *compatibility*)
                           0 0 (length starts) (length starts)
                           (- cd-end cd-start) cd-start #())
                          output)
        (write-structure* (make-end-of-central-directory-locator/64
                           0 cd-end 1)
                          output)
        (write-structure* (make-end-of-central-directory
                           0 0
                           (cap (length starts) 16)
                           (cap (length starts) 16)
                           (cap (- cd-end cd-start) 32)
                           (cap cd-start 32)
                           (length comment) comment)
                          output)))))
