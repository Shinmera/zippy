#|
 This file is a part of zippy
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.zippy)

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

(defun decode-extra-fields (vector index length)
  (let ((end (+ index length)))
    (values (loop while (< index end)
                  for sig = (nibbles:ub16/le vector index)
                  for dec = (gethash sig *structures*)
                  if dec
                  collect (multiple-value-bind (struct ind)
                              (funcall (first dec) vector (+ index 2))
                            (setf index ind)
                            struct)
                  else
                  do (incf index (+ 4 (nibbles:ub16/le vector index))))
            index)))

(defun decode (input)
  (let* ((file (make-instance 'zip-file))
         (entries (file-entries file)))
    ;; First seek to end of file, then backtrack to find the end-of-central-directory signature.
    ;; We skip the bytes that are guaranteed to be part of the structure anyway. Thus, if the
    ;; comment is empty, we should immediately end up at the signature.
    (seek input (- (size input) (+ 4 2 2 2 2 4 4 2)))
    (loop for byte = (ub32 input)
          until (= #x06054B50 byte)
          ;; Seek back the 4 bytes we read +1 extra byte.
          ;; TODO: This could be sped up by trying to match parts of the signature against what we
          ;;       read and then speculatively back up more bytes.
          ;; TODO: Check for trying to seek out of the beginning of the file.
          do (seek input (- (index input) 5)))
    ;; We should now be at the beginning (after the signature) of the end-of-central-directory.
    (let ((eocd-start (- (index input) 4))
          (eocd (parse-structure end-of-central-directory input))
          (cd-start 0) (cd-entries 0))
      ;; OK, next we look for end-of-central-directory-locator/64, which should be eocd-start-16
      (seek input (- eocd-start 16))
      (when (= #x07064B50 (ub32 input))
        (let ((eocd-locator (parse-structure end-of-central-directory-locator/64 input)))
          (when (= (end-of-central-directory-number-of-disk eocd)
                   (end-of-central-directory-locator/64-central-directory-disk eocd-locator))
            ;; Okey, header is on here, let's check it.
            (seek input (end-of-central-directory-locator/64-central-directory-start eocd-locator))
            (when (= #x06064B50 (ub32 input))
              (let ((eocd/64 (parse-structure end-of-central-directory/64 input)))
                )))))
      
      (cond ((= (end-of-central-directory-number-of-disk eocd)
                (end-of-central-directory-central-directory-disk eocd))
             ;; We are on the primary disk. Seek to the beginning of the entries.
             (seek input (end-of-central-directory-central-directory-start eocd))
             (loop repeat (end-of-central-directory-central-directory-entries eocd)
                   for structure = (parse-structure* input)
                   for entry = (make-instance 'zip-entry)
                   do (decode-central-directory-entry structure entry)
                      (vector-push-extend entry entries)))
            (T
             ;; FIXME: How the fuck do we figure out where the central directory entries start?
             (error "Fuck")))
      ))
  
  (multiple-value-bind (structure index) (decode-structure vector index)
    (typecase structure
      (local-file
       (let ((entry (make-instance 'zip-entry)))
         (setf (attribute-compatibility entry) (gethash (ldb (byte 8 8) (local-file-version structure))
                                                        *file-attribute-compatibility-map*))
         (multiple-value-bind (major minor) (floor (local-file-version structure) 10)
           (setf (version entry) (list major minor)))
         (multiple-value-bind (fields new-index)
             (decode-extra-fields vector index (local-file-extra-field-length structure))
           (setf index new-index)
           (setf (extra-fields entry) fields))
         (let ((enc (if (logbitp 11 (local-file-flags structure)) :utf-8 :cp437)))
           (setf (file-name structure) (babel:octets-to-string (local-file-file-name) :encoding enc)))
         (when (logbitp 3 (local-file-flags record))
           (error "Wtf?"))))
      (T))))
