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
            T ;; FIXME: Should only set this to T if the /output/ is non-seekable
            NIL
            NIL
            (and (encryption-method entry)
                 (not (eql :pkware (encryption-method entry))))
            NIL NIL NIL NIL T NIL NIL NIL NIL))

(defun backfill-from-content (entry)
  (let ((content (content entry)))
    ;; FIXME: File attributes (needs non-standard API :/)
    (etypecase content
      (file-stream
       (setf (last-modified entry) (file-write-date content))
       (setf (uncompressed-size entry) (file-length content))
       (unless (file-name entry)
         (setf (file-name entry) (file-namestring content))))
      (pathname
       (setf (last-modified entry) (file-write-date content))
       (unless (file-name entry)
         (setf (file-name entry) (file-namestring content))))
      (stream)
      (vector-input
       (setf (uncompressed-size entry) (size content)))
      (vector
       (setf (uncompressed-size entry) (length content))))
    (when (and (null (compression-method entry))
               (< 1024 (or (uncompressed-size entry) 1025)))
      (setf (compression-method entry) :deflate))))

(defun entry-version (entry)
  (encode-version (or (version entry) *version*)
                  (if (consp (attributes entry))
                      (first (attributes entry))
                      *compatibility*)))

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
      (make-local-file (entry-version entry)
                       (entry-flags entry) (compression-method entry)
                       date time (or (crc-32 entry) 0)
                       (if (size entry) (cap (size entry) 32))
                       (if (uncompressed-size entry) (cap (uncompressed-size entry) 32))
                       (length file-name) (length extra) file-name extra))))

(defun entry-to-dd (entry)
  (if (< (uncompressed-size entry) #xFFFFFFFF)
      (make-data-descriptor (crc-32 entry) (size entry) (uncompressed-size entry))
      (make-data-descriptor/64 (crc-32 entry) (size entry) (uncompressed-size entry))))

(defun entry-to-cd (entry)
  (multiple-value-bind (date time) (encode-msdos-timestamp (last-modified entry))
    (let ((file-name (babel:octets-to-string (file-name entry) :encoding :utf-8))
          (comment (if (comment entry)
                       (babel:octets-to-string (comment entry) :encoding :utf-8)
                       #()))
          (extra #()))
      (when (or (<= #xFFFFFFFF (size entry))
                (<= #xFFFFFFFF (start entry)))
        (setf extra (make-array 20 :element-type '(unsigned-byte 8)))
        (encode-structure (make-zip64-extended-information
                           28 (size entry) (uncompressed-size entry)
                           (start entry) 0)
                          extra 0))
      (make-central-directory-entry
       (entry-version entry)
       (entry-version entry)
       (entry-flags entry)
       (compression-method entry)
       date time (or (crc-32 entry) 0)
       (if (size entry) (cap (size entry) 32))
       (if (uncompressed-size entry) (cap (uncompressed-size entry) 32))
       (length file-name) (length extra) (length comment)
       0 0 (or (second (attributes entry)) 0) (cap (start entry) 32)
       file-name extra comment))))

(defun encode-entry-payload (entry output password)
  (with-io (input (content entry))
    (let ((read 0)
          (written 0)
          (crc #xFFFFFFFF)
          (encryption-state (make-encryption-state (encryption-method entry) password))
          (compression-state (make-compression-state (compression-method entry))))
      (labels ((write-out (buffer start end)
                 (incf written (- end start))
                 (output output buffer start end))
               (encrypt (buffer start end)
                 (call-with-encrypted-buffer #'write-out buffer start end encryption-state))
               (compress (buffer start end)
                 (incf read (- end start))
                 (loop for i from start below end
                       do (setf crc (crc32-rotate crc (aref buffer i))))
                 (call-with-compressed-buffer #'encrypt buffer start end compression-state)))
        (etypecase input
          (stream
           (loop with buffer = (make-array 4096 :element-type '(unsigned-byte 8))
                 for read = (read-sequence buffer input)
                 while (< 0 read)
                 do (compress buffer 0 read)))
          (vector-input
           (compress (vector-input-vector input) (vector-input-index input) (size (vector-input-vector input))))))
      (setf (crc-32 entry) (logxor #xFFFFFFFF crc))
      (setf (size entry) written)
      (setf (uncompressed-size entry) read))))

(defun encode (zip-file output &key password)
  (loop for i from 0
        for entry across (entries zip-file)
        do (setf (start entry) (index output))
           (backfill-from-content entry)
           (write-structure* (entry-to-lf entry) output)
           (encode-entry-payload entry output password)
           (write-structure* (entry-to-dd entry) output)
           ;; FIXME: If writing to a file-stream or vector, backtrack and
           ;;        Fixup the LF entry with new info
        )
  (let ((cd-start (index output)))
    (loop for entry across (entries zip-file)
          do (write-structure* (entry-to-cd entry) output))
    (let ((cd-end (index output))
          (comment (or (comment zip-file) "")))
      (write-structure* (make-end-of-central-directory/64
                         44
                         (encode-version *version* *compatibility*)
                         ;; FIXME: be smarter about noting the min version.
                         (encode-version *version* *compatibility*)
                         0 0 (length (entries zip-file)) (length (entries zip-file))
                         (- cd-end cd-start) cd-start #())
                        output)
      (write-structure* (make-end-of-central-directory-locator/64
                         0 cd-end 1)
                        output)
      (write-structure* (make-end-of-central-directory
                         0 0
                         (cap (length (entries zip-file)) 16)
                         (cap (length (entries zip-file)) 16)
                         (cap (- cd-end cd-start) 32)
                         (cap cd-start 32)
                         (length comment) comment)
                        output))))
