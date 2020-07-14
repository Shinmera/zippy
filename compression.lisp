#|
 This file is a part of zippy
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.zippy)

(defgeneric call-with-decompressed-buffer (function input length format &key buffer))
(defgeneric call-with-compressed-buffer (function input length format &key buffer))

(defmethod decompress-vector (vector index length (format (eql :deflate)))
  (3bz:decompress-vector vector :format :deflate :start index :end (+ index length)))

(defun ensure-buffer (buffer)
  (etypecase buffer
    (vector buffer)
    (integer (make-array buffer :element-type '(unsigned-byte 8)))
    (null (make-array 4096 :element-type '(unsigned-byte 8)))))

(defmethod call-with-decompressed-buffer (function (input stream) length (format (eql NIL)) &key buffer)
  (let ((buffer (ensure-buffer buffer)))
    (loop while (< 0 length)
          for read = (read-sequence buffer input :end (min (length buffer) length))
          do (funcall function buffer 0 read)
             (decf length read))))

(defmethod call-with-decompressed-buffer (function (input vector-input) length (format (eql NIL)) &key buffer)
  (declare (ignore buffer))
  (let ((start (vector-input-index input)))
    (funcall function (vector-input-vector input) start (+ start length))))

(defmethod call-with-decompressed-buffer (function (input stream) length (format (eql :deflate)) &key buffer)
  (let* ((buffer (ensure-buffer buffer))
         (state (3bz:make-deflate-state :output-buffer buffer))
         (context (3bz:make-octet-stream-context input)))
    (loop for read = (3bz:decompress context state)
          do (funcall function buffer 0 read)
             (cond ((3bz:finished state) (return))
                   ((3bz:input-underrun state)
                    (error "WTF"))
                   ((3bz:output-overflow state)
                    (3bz:replace-output-buffer state buffer))))))

(defmethod call-with-decompressed-buffer (function (input vector-input) length (format (eql :deflat)) &key buffer)
  (let* ((buffer (ensure-buffer buffer))
         (state (3bz:make-deflate-state :output-buffer buffer))
         (context (3bz:make-octet-vector-context (vector-input-vector input)
                                                 :start (vector-input-index input)
                                                 :end (+ (vector-input-index input) length))))
    (loop for read = (3bz:decompress context state)
          do (funcall function buffer 0 read)
             (cond ((3bz:finished state) (return))
                   ((3bz:input-underrun state)
                    (error "WTF"))
                   ((3bz:output-overflow state)
                    (3bz:replace-output-buffer state buffer))))))
