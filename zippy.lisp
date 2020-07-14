#|
 This file is a part of zippy
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.zippy)

(define-condition archive-file-required (error)
  ((id :initarg :id :initform (error "ID required.")))
  (:report (lambda (c s) (format s "Disk ~a is required to continue reading the Zip file."
                                 (slot-value c 'id)))))

(defclass zip-file ()
  ((entries :initarg :entries :accessor entries)
   (disks :initarg :disks :accessor disks)))

(defmethod close ((file zip-file) &key abort)
  (when (disks file)
    (loop for disk across (disks file)
          do (when (streamp disk)
               (close disk :abort abort)))
    (setf (disks file) NIL)))

(defmethod print-object ((file zip-file) stream)
  (let ((disk (when (disks file) (aref (disks file) (1- (length (disks file)))))))
    (print-unreadable-object (file stream :type T)
      (etypecase disk
        (stream (if (open-stream-p disk)
                    (format stream "~s" (pathname disk))
                    (format stream "CLOSED")))
        (vector-input (format stream "[VECTOR]"))
        (null (format stream "CLOSED"))))))

(defclass zip-entry ()
  ((zip-file :initform NIL :accessor zip-file)
   (version :initform NIL :accessor version)
   (file-type :initform NIL :accessor file-type)
   (attributes :initform NIL :accessor attributes)
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

(defun decode-extra-fields (vector)
  (let ((fields ()))
    (loop with index = 0
          while (< index (length vector))
          do (let* ((sig (nibbles:ub16ref/le vector index))
                    (dec (gethash sig *structures*)))
               (incf index 2)
               (when dec
                 (push (funcall (first dec) vector index) fields))
               (if (< index (length vector))
                   (incf index (nibbles:ub16ref/le vector index))
                   (return))))
    (nreverse fields)))

(defun dbg (f &rest args)
  (format T "~& -> ~?~%" f args))

(defun process-extra-field (entry field)
  (typecase field
    (zip64-extended-information
     (setf (size entry) (zip64-extended-information-compressed-size field))
     (setf (uncompressed-size entry) (zip64-extended-information-original-size field))
     (setf (start entry) (zip64-extended-information-header-offset field))
     (setf (disk entry) (zip64-extended-information-starting-disk field)))
    (encryption-header
     (setf (encryption-method entry)
           (list (gethash (encryption-header-encryption-algorithm field) *encryption-method-map*)
                 (encryption-header-bit-length field))))))

(defun lf-to-entry (lf entry)
  (macrolet ((maybe-set (field value)
               `(let ((value ,value))
                  (cond ((null (,field entry))
                         (setf (,field entry) value))
                        ((not (equal value (,field entry)))
                         (warn "Mismatch in ~a:~%  Central directory: ~a~%  Local file header: ~a"
                               ',field (,field entry) value))))))
    (maybe-set version (decode-version (local-file-version lf)))
    (setf (crc-32 entry) (local-file-crc-32 lf))
    ;; Ignore if size is not contained or in zip64
    (unless (or (logbitp 3 (local-file-flags lf))
                (= #xFFFFFFFF (local-file-compressed-size lf)))
      (maybe-set size (local-file-compressed-size lf))
      (maybe-set uncompressed-size (local-file-uncompressed-size lf)))
    (maybe-set compression-method (aref *compression-method-map* (local-file-compression-method lf)))
    (maybe-set encryption-method (cond ((logbitp 6 (local-file-flags lf)) :strong)
                                       ((logbitp 0 (local-file-flags lf)) :pkware)))
    (maybe-set file-name (decode-string (local-file-file-name lf) (local-file-flags lf)))
    (setf (extra-fields entry) (append (extra-fields entry) (decode-extra-fields (local-file-extra lf))))
    (loop for field in (extra-fields entry)
          do (process-extra-field entry field))))

(defun cde-to-entry (cde entry)
  (setf (version entry) (decode-version (central-directory-entry-version-needed cde)))
  (setf (attributes entry) (list (aref *file-attribute-compatibility-map*
                                       (ldb (byte 8 8) (central-directory-entry-version-made cde)))
                                 (central-directory-entry-external-file-attributes cde)))
  (setf (crc-32 entry) (central-directory-entry-crc-32 cde))
  (setf (size entry) (central-directory-entry-compressed-size cde))
  (setf (uncompressed-size entry) (central-directory-entry-uncompressed-size cde))
  (setf (start entry) (central-directory-entry-local-header-offset cde))
  (setf (disk entry) (central-directory-entry-disk-number-start cde))
  (setf (last-modified entry) (decode-msdos-timestamp (central-directory-entry-last-modified-date cde)
                                                      (central-directory-entry-last-modified-time cde)))
  (setf (compression-method entry) (aref *compression-method-map* (central-directory-entry-compression-method cde)))
  (setf (encryption-method entry) (cond ((logbitp 6 (central-directory-entry-flags cde)) :strong)
                                        ((logbitp 0 (central-directory-entry-flags cde)) :pkware)))
  (setf (comment entry) (decode-string (central-directory-entry-file-comment cde)
                                       (central-directory-entry-flags cde)))
  (setf (file-name entry) (decode-string (central-directory-entry-file-name cde)
                                         (central-directory-entry-flags cde)))
  (setf (extra-fields entry) (decode-extra-fields (central-directory-entry-extra cde)))
  (loop for field in (extra-fields entry)
        do (process-extra-field entry field)))

(defun decode-central-directory (input entries entry-offset)
  (let ((i entry-offset))
    (loop for structure = (parse-structure* input)
          for entry = (make-instance 'zip-entry)
          do (cde-to-entry structure entry)
             (setf (aref entries i) entry)
             (incf i)
          while (and (has-more input)
                     (< i (length entries))))
    i))

(defun decode (input)
  (let (entries disks)
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
    (let* ((eocd (parse-structure end-of-central-directory input))
           (cd-offset (end-of-central-directory-central-directory-start eocd))
           (cd-start-disk (end-of-central-directory-central-directory-disk eocd))
           (cd-end-disk (end-of-central-directory-number-of-disk eocd)))
      ;; OK, next we look for end-of-central-directory-locator/64, which should be
      ;; input - 4 (eocd sig) - 16 (ecod64 payload) - 4 (eocd64 sig)
      (seek input (- (index input) 4 16 4))
      (when (= #x07064B50 (ub32 input))
        (let ((eocd-locator (parse-structure end-of-central-directory-locator/64 input))
              (eocd64-input input))
          (when (/= (end-of-central-directory-number-of-disk eocd)
                    (end-of-central-directory-locator/64-central-directory-disk eocd-locator))
            (restart-case (error 'archive-file-required :id (end-of-central-directory-locator/64-central-directory-disk eocd-locator))
              (use-value (new-input)
                (setf eocd64-input new-input))))
          (setf disks (make-array (end-of-central-directory-locator/64-number-of-disks eocd-locator) :initial-element NIL))
          (setf (aref disks (end-of-central-directory-locator/64-central-directory-disk eocd-locator)) eocd64-input)
          ;; Okey, header is on here, let's check it.
          (seek eocd64-input (end-of-central-directory-locator/64-central-directory-start eocd-locator))
          (if (= #x06064B50 (ub32 eocd64-input))
              (let ((eocd (parse-structure end-of-central-directory/64 eocd64-input)))
                (setf cd-offset (end-of-central-directory/64-central-directory-start eocd))
                (setf cd-start-disk (end-of-central-directory/64-central-directory-disk eocd))
                (setf cd-end-disk (end-of-central-directory/64-number-of-disk eocd))
                (setf entries (make-array (end-of-central-directory/64-central-directory-entries eocd) :initial-element NIL)))
              (warn "File appears corrupted: 

Zip64 End of Central Directory Record was not at indicated position.
Will attempt to continue with 32 bit standard central directory."))))
      (cond ((and (null entries) (= #xFFFFFFFF (end-of-central-directory-central-directory-start eocd)))
             (error "File appears corrupted:

No Zip64 End of Central Directory record found, but End of Central
Directory contains a start marker that indicates there should be
one."))
            (T
             (let ((i 0))
               (unless entries
                 (setf entries (make-array (end-of-central-directory-central-directory-entries eocd) :initial-element NIL)))
               (unless disks
                 (setf disks (make-array (1+ (end-of-central-directory-number-of-disk eocd)) :initial-element NIL)))
               (unless (= #xFFFF (end-of-central-directory-number-of-disk eocd))
                 (setf (aref disks (end-of-central-directory-number-of-disk eocd)) input))
               (loop for disk from cd-start-disk to cd-end-disk
                     for input = (or (aref disks disk)
                                     (restart-case (error 'archive-file-required :id disk)
                                       (use-value (new-input)
                                         (setf (aref disks disk) new-input))))
                     do (seek input cd-offset)
                        (setf cd-offset 0)
                        (setf i (decode-central-directory input entries i))))))
      (let ((zip-file (make-instance 'zip-file :entries entries :disks disks)))
        (loop for entry across entries
              do (setf (zip-file entry) zip-file))
        zip-file))))

(defun call-with-input-zip-file (function input &key (start 0))
  (etypecase input
    ((or pathname string)
     (let ((streams ()))
       (handler-bind ((archive-file-required
                        (lambda (c)
                          (let ((id (slot-value c 'id)))
                            (let ((stream (open (make-pathname :type (format NIL "z~2,'0d" (1+ id)) :defaults input)
                                                :element-type '(unsigned-byte 8))))
                              (push stream streams)
                              (use-value stream))))))
         (with-open-file (stream input :element-type '(unsigned-byte 8))
           (unwind-protect
                (let ((file (decode stream)))
                  (funcall function file))
             (mapc #'close streams))))))
    (stream
     (funcall function (decode input)))
    (vector
     (funcall function (decode (make-vector-input input start))))))

(defmacro with-zip-file ((file input &key (start 0)) &body body)
  `(call-with-input-zip-file (lambda (,file) ,@body) ,input :start ,start))

(defun decode-entry (function entry &key password)
  (let ((input (aref (disks (zip-file entry)) (disk entry))))
    (seek input (start entry))
    (lf-to-entry (parse-structure* input) entry)
    ;; Now at beginning of the data payload
    (let ((decryption-state (make-decryption-state (encryption-method entry) input password))
          (decompression-state (make-decompression-state (compression-method entry))))
      (flet ((decompress (buffer start end)
               (call-with-decompressed-buffer function buffer start end decompression-state)))
        (call-with-decrypted-buffer #'decompress input (size entry) decryption-state)))))

(defun entry-to-file (path entry &key (if-exists :error) password)
  (with-open-file (stream path :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists if-exists)
    (flet ((output (buffer start end)
             (write-sequence buffer stream :start start :end end)))
      (decode-entry #'output entry :password password))))

(defun extract-zip (file path &key (if-exists :error) password)
  (etypecase file
    (zip-file
     (loop for entry across (entries file)
           for full-path = (merge-pathnames (file-name entry) path)
           do (ensure-directories-exist full-path)
              (entry-to-file full-path entry :if-exists if-exists :password password)))
    (T
     (with-zip-file (zip file)
       (extract-zip path zip :if-exists if-exists)))))
