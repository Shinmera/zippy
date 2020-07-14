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
   (version :initform NIL :accessor version)
   (attributes :initform NIL :accessor attributes)
   (encryption-method :initform NIL :accessor encryption-method)
   (compression-method :initform NIL :accessor compression-method)
   (crc-32 :initform NIL :accessor crc-32)
   (disk :initform NIL :accessor disk)
   (start :initform NIL :accessor start)
   (size :initform NIL :accessor size)
   (uncompressed-size :initform NIL :accessor uncompressed-size)
   (extra-fields :initform NIL :accessor extra-fields)
   (last-modified :initform (get-universal-time) :initarg :last-modified :accessor last-modified)
   (file-name :initform NIL :initarg :file-name :accessor file-name)
   (comment :initform NIL :initarg :comment :accessor comment)
   (content :initform NIL :initarg :content :accessor content)))

(defmethod print-object ((entry zip-entry) stream)
  (print-unreadable-object (entry stream :type T)
    (format stream "~s" (file-name entry))))

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

(defun compress-zip (file target &key (start 0) (if-exists :error) password)
  (etypecase target
    ((or string pathname)
     (with-open-file (stream target :direction :output
                                    :element-type '(unsigned-byte 8)
                                    :if-exists if-exists)
       (encode file stream :password password)))
    (stream
     (encode file target :password password))
    (vector
     (encode file (make-vector-input target start) :password password))))
