#|
 This file is a part of zippy
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.zippy)

(defclass zip-file ()
  (entries))

(defclass zip-entry ()
  ((version :initform NIL :accessor version)
   (attribute-compatibility :initform NIL :accessor attribute-compatibility)
   (encryption-method :initform NIL :accessor encryption-method)
   (compression-method :initform NIL :accessor compression-method)
   (last-modified :initform NIL :accessor last-modified)
   (crc-32 :initform NIL :accessor crc-32)
   (disk :initform NIL :accessor disk)
   (start :initform NIL :accessor start)
   (size :initform NIL :accessor size)
   (uncompressed-size :initform NIL :accessor uncompressed-size)
   (file-name :initform NIL :accessor file-name)
   (extra-fields :initform NIL :accessor extra-fields)
   (comment :initform NIL :accessor comment)))

(defmethod print-object ((entry zip-entry) stream)
  (print-unreadable-object (entry stream :type T)
    (format stream "~s" (file-name entry))))

(defun decode-extra-fields (vector index length)
  (let ((end (+ index length)))
    (values (loop while (< index end)
                  for sig = (nibbles:ub16ref/le vector index)
                  for dec = (gethash sig *structures*)
                  if dec
                  collect (multiple-value-bind (struct ind)
                              (funcall (first dec) vector (+ index 2))
                            (setf index ind)
                            struct)
                  else
                  do (incf index (+ 4 (nibbles:ub16ref/le vector index))))
            index)))

(defun dbg (f &rest args)
  (format T "~& -> ~?~%" f args))

(defun lf-to-entry (lf entry)
  (macrolet ((maybe-set (field value)
               `(unless (,field entry)
                  (setf (,field entry) ,value))))
    (maybe-set version (decode-version (local-file-version lf)))
    (maybe-set attribute-compatibility (aref *file-attribute-compatibility-map*
                                             (ldb (byte 8 8) (local-file-version lf))))
    (setf (crc-32 entry) (local-file-crc-32 lf))
    (unless (logbitp 3 (local-file-flags lf))
      (maybe-set size (local-file-compressed-size entry))
      (maybe-set uncompressed-size (local-file-uncompressed-size entry)))
    (maybe-set compression-method (aref *compression-method-map* (local-file-compression-method lf)))
    (maybe-set encryption-method (logbitp 0 (local-file-flags lf)))
    (maybe-set file-name (decode-string (local-file-file-name lf) (local-file-flags lf)))
    (setf (extra-fields entry) (append (extra-fields entry) (decode-extra-fields (local-file-extra lf) 0
                                                                                 (local-file-extra-field-length lf))))))

(defun cde-to-entry (cde entry)
  (setf (version entry) (decode-version (central-directory-entry-version-made cde)))
  (setf (attribute-compatibility entry) (aref *file-attribute-compatibility-map*
                                              (ldb (byte 8 8) (central-directory-entry-version-made cde))))
  (setf (crc-32 entry) (central-directory-entry-crc-32 cde))
  (setf (size entry) (central-directory-entry-compressed-size cde))
  (setf (uncompressed-size entry) (central-directory-entry-uncompressed-size cde))
  (setf (start entry) (central-directory-entry-local-header-offset cde))
  (setf (disk entry) (central-directory-entry-disk-number-start cde))
  (setf (last-modified entry) (decode-msdos-timestamp (central-directory-entry-last-modified-date cde)
                                                      (central-directory-entry-last-modified-time cde)))
  (setf (compression-method entry) (aref *compression-method-map* (central-directory-entry-compression-method cde)))
  (setf (encryption-method entry) (logbitp 0 (central-directory-entry-flags cde)))
  (setf (comment entry) (decode-string (central-directory-entry-file-comment cde)
                                       (central-directory-entry-flags cde)))
  (setf (file-name entry) (decode-string (central-directory-entry-file-name cde)
                                         (central-directory-entry-flags cde)))
  (setf (extra-fields entry) (decode-extra-fields (central-directory-entry-extra cde) 0
                                                  (central-directory-entry-extra-field-length cde)))
  (loop for field in (extra-fields entry)
        do (typecase field
             (zip64-extended-information
              (setf (size entry) (zip64-extended-information-compressed-size field))
              (setf (uncompressed-size entry) (zip64-extended-information-original-size field))
              (setf (start entry) (zip64-extended-information-header-offset field))
              (setf (disk entry) (zip64-extended-information-starting-disk field))))))

(defun decode-eocd-entries (input entries)
  (loop for i from 0 below (length entries)
        for structure = (parse-structure* input)
        for entry = (make-instance 'zip-entry)
        do (cde-to-entry structure entry)
           (setf (aref entries i) entry)))

(defun decode (input)
  (let (entries)
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
    (let ((eocd (parse-structure end-of-central-directory input)))
      ;; OK, next we look for end-of-central-directory-locator/64, which should be
      ;; input - 4 (eocd sig) - 16 (ecod64 payload) - 4 (eocd64 sig)
      (seek input (- (index input) 4 16 4))
      (when (= #x07064B50 (ub32 input))
        (let ((eocd-locator (parse-structure end-of-central-directory-locator/64 input)))
          (when (/= (end-of-central-directory-number-of-disk eocd)
                    (end-of-central-directory-locator/64-central-directory-disk eocd-locator))
            (restart-case (error "FIXME: Supply disk ~a" (end-of-central-directory-locator/64-central-directory-disk eocd-locator))
              (use-value (new-input)
                (setf input new-input))))
          ;; Okey, header is on here, let's check it.
          (seek input (end-of-central-directory-locator/64-central-directory-start eocd-locator))
          (when (= #x06064B50 (ub32 input))
            ;; If we had not found the header we would fall back to standard EOCD parsing.
            (let ((eocd/64 (parse-structure end-of-central-directory/64 input)))
              (setf entries (make-array (end-of-central-directory/64-central-directory-entries eocd/64)))
              (seek input (end-of-central-directory/64-central-directory-start eocd/64))
              (decode-eocd-entries input entries)))))
      (cond (entries
             #++"Zip64 EOCD already populated the entries, we're good.")
            ((= #xFFFFFFFF (end-of-central-directory-central-directory-start eocd))
             (error "File appears corrupted:

No Zip64 End of Central Directory record found, but End of Central
Directory contains a start marker that indicates there should be
one."))
            (T
             (when (/= (end-of-central-directory-number-of-disk eocd)
                       (end-of-central-directory-central-directory-disk eocd))
               (restart-case (error "FIXME: Supply disk ~a" (end-of-central-directory-central-directory-disk eocd))
                 (use-value (new-input)
                   (setf input new-input))))
             (setf entries (make-array (end-of-central-directory-central-directory-entries eocd)))
             (seek input (end-of-central-directory-central-directory-start eocd))
             (decode-eocd-entries input entries)))
      entries)))
