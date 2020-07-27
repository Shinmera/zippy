#|
 This file is a part of zippy
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.zippy)

(defgeneric make-decompression-state (format &key buffer))
(defgeneric call-with-decompressed-buffer (function vector start end state))

(defgeneric make-compression-state (format &key buffer))
(defgeneric call-with-compressed-buffer (function vector start end state))
(defgeneric call-with-completed-compressed-buffer (function state))

(defmethod make-decompression-state (format &key buffer)
  (error "Unsupported compression method: ~a" format))

(defmethod make-decompression-state ((format (eql NIL)) &key buffer)
  NIL)

(defmethod call-with-decompressed-buffer (function input start end (state (eql NIL)))
  (funcall function input start end))

(defmethod make-decompression-state ((format (eql :deflate)) &key buffer)
  (3bz:make-deflate-state :output-buffer (ensure-buffer buffer)))

(defmethod call-with-decompressed-buffer (function input start end (state 3bz::deflate-state))
  (loop with context = (3bz:make-octet-vector-context input :start start :end end)
        for read = (3bz:decompress context state)
        do (funcall function (3bz::ds-output-buffer state) 0 read)
           (cond ((3bz:finished state) (return :finish))
                 ((3bz:input-underrun state) (return :need-more))
                 ((3bz:output-overflow state)
                  (3bz:replace-output-buffer state (3bz::ds-output-buffer state))))))

(defmethod make-compression-state ((format (eql NIL)) &key buffer)
  NIL)

(defmethod call-with-compressed-buffer (function vector start end (state null))
  (funcall function vector start end))

(defmethod call-with-completed-compressed-buffer (function (state (eql NIL)))
  (funcall function #() 0 0))

(defmethod make-compression-state ((format (eql :deflate)) &key buffer)
  (make-instance 'salza2:deflate-compressor))

(defmethod call-with-compressed-buffer (function vector start end (state salza2:deflate-compressor))
  (setf (salza2:callback state) (lambda (buffer end) (funcall function buffer 0 end)))
  (salza2:compress-octet-vector vector state :start start :end end))

(defmethod call-with-completed-compressed-buffer (function (state salza2:deflate-compressor))
  (setf (salza2:callback state) (lambda (buffer end) (funcall function buffer 0 end)))
  (salza2:finish-compression state))
