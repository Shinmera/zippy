#|
 This file is a part of zippy
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.zippy)

(defclass zip-file ()
  ((entries :initarg :entries :accessor entries)
   (disks :initarg :disks :accessor disks)
   (comment :initform NIL :initarg :comment :accessor comment)))

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
   (crc-32 :initform NIL :accessor crc-32)
   (disk :initform NIL :accessor disk)
   (offset :initform NIL :accessor offset)
   (size :initform NIL :accessor size)
   (uncompressed-size :initform NIL :accessor uncompressed-size)
   (extra-fields :initform NIL :accessor extra-fields)
   (version :initform NIL :initarg :version :accessor version)
   (attributes :initform NIL :initarg :attributes :accessor attributes)
   (encryption-method :initform NIL :initarg :encryption-method :accessor encryption-method)
   (compression-method :initform NIL :initarg :compression-method :accessor compression-method)
   (last-modified :initform (get-universal-time) :initarg :last-modified :accessor last-modified)
   (file-name :initform NIL :initarg :file-name :accessor file-name)
   (comment :initform NIL :initarg :comment :accessor comment)
   (content :initform NIL :initarg :content :accessor content)))

(defmethod print-object ((entry zip-entry) stream)
  (print-unreadable-object (entry stream :type T)
    (format stream "~s" (file-name entry))))

(defun entry-to-file (path entry &key (if-exists :error) password (restore-attributes T))
  (with-open-file (stream path :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists if-exists)
    (flet ((output (buffer start end)
             (write-sequence buffer stream :start start :end end)))
      (decode-entry #'output entry :password password)))
  (when (and restore-attributes
             (eql *compatibility* (second (attributes entry))))
    ;; TODO: restore other extended attributes from the extra blocks (uid/gid/etc)
    (setf (file-attributes:attributes path) (third (attributes entry)))))

(defun entry-to-stream (stream entry &key password)
  (flet ((output (buffer start end)
           (write-sequence buffer stream :start start :end end)))
    (decode-entry #'output entry :password password)))

(defun entry-to-vector (entry &key vector (start 0) password)
  (let ((vector (etypecase vector
                  ((vector (unsigned-byte 8)) vector)
                  (null (make-array (uncompressed-size entry) :element-type '(unsigned-byte 8)))))
        (i start))
    (flet ((fast-copy (buffer start end)
             #+sbcl
             (sb-sys:with-pinned-objects (vector buffer)
               (sb-kernel:system-area-ub8-copy (sb-sys:vector-sap buffer) start (sb-sys:vector-sap vector) i (- end start))
               (incf i (- end start))))
           (slow-copy (buffer start end)
             (loop for j from start below end
                   do (setf (aref vector i) (aref buffer j))
                      (incf i))))
      (if #+sbcl (typep vector 'sb-kernel:simple-unboxed-array)
          #-sbcl NIL
          (decode-entry #'fast-copy entry :password password)
          (decode-entry #'slow-copy entry :password password))
      vector)))

;; Early define
(defmacro with-zip-file ((file input &key (start 0) end) &body body)
  `(call-with-input-zip-file (lambda (,file) ,@body) ,input :start ,start :end ,end))

(defun extract-zip (file path &key (if-exists :error) password)
  (etypecase file
    (zip-file
     (loop for entry across (entries file)
           for full-path = (merge-pathnames (file-name entry) path)
           do (ensure-directories-exist full-path)
              (unless (getf (first (attributes entry)) :directory)
                (entry-to-file full-path entry :if-exists if-exists :password password))))
    (T
     (with-zip-file (zip file)
       (extract-zip zip path :if-exists if-exists)))))

(defun ensure-zip-file (file)
  (etypecase file
    ((or pathname string)
     (let ((entries (make-array 0 :adjustable T :fill-pointer T)))
       (if (or (pathname-name file) (pathname-type file))
           (vector-push-extend (make-instance 'zip-entry :content file) entries)
           (loop with base = (pathname-utils:parent file)
                 for path in (directory (merge-pathnames (merge-pathnames pathname-utils:*wild-file* pathname-utils:*wild-inferiors*)
                                                         file))
                 for file-name = (enough-namestring path base)
                 do (vector-push-extend (make-instance 'zip-entry :content path :file-name file-name) entries)))
       (make-instance 'zip-file :entries entries :comment "Created with Zippy")))
    ((or vector stream)
     (let ((entries (make-array 1)))
       (setf (aref entries 0) (make-instance 'zip-file :content file :file-name "-"))
       (make-instance 'zip-file :entries entries :comment "Created with Zippy")))
    (zip-file
     file)))

(defun compress-zip (file target &key (start 0) end (if-exists :error) password)
  (let ((file (ensure-zip-file file)))
    (with-io (io target :direction :output :if-exists if-exists :start start :end end)
      (encode-file file io :password password))))
