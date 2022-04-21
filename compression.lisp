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

(defmethod make-decompression-state ((format (eql :store)) &key buffer)
  NIL)

(defmethod call-with-decompressed-buffer (function input start end (state (eql NIL)))
  (funcall function input start end))

(defstruct (deflate-state (:include 3bz::deflate-state))
  (last-read 0 :type (signed-byte 32))
  (available 0 :type (signed-byte 32))
  (input-state NIL :type T))

(defmethod make-decompression-state ((format (eql :deflate)) &key buffer)
  (make-deflate-state :output-buffer (ensure-buffer buffer)))

(defun crc32-array (array start end)
  (let ((crc #xFFFFFFFF))
    (loop for i from start below end
          do (setf crc (crc32-rotate crc (aref array i))))
    crc))

(defun test (&key (file "~/test") (bufsize 1024))
  (let ((safe (alexandria:read-file-into-byte-vector (make-pathname :type "dat" :defaults file))))
    (org.shirakumo.zippy:with-zip-file (file (make-pathname :type "zip" :defaults file))
      (let* ((entry (aref (org.shirakumo.zippy:entries file) 0))
             (decoder (org.shirakumo.zippy::make-chunk-decoder entry))
             (buf (make-array bufsize :element-type '(unsigned-byte 8)))
             (i 0))
        (format T "~&Zip reports ~8d bytes uncompressed, ~8d bytes compressed."
                (org.shirakumo.zippy:uncompressed-size entry)
                (org.shirakumo.zippy:size entry))
        (loop while (< i (length safe))
              for read = (org.shirakumo.zippy::decode-chunk decoder buf 0 (length buf))
              do (loop with mismatches = 0
                       for j from 0 below read
                       while (< i (length safe))
                       do (unless (= (aref buf j) (aref safe i))
                            (format T "~&Mismatch at ~8d (~4d): ~3d found ~3d expected"
                                    i j (aref buf j) (aref safe i))
                            (when (<= 10 (incf mismatches))
                              (return-from test)))
                          (incf i)))))))

(defmethod call-with-decompressed-buffer (function input start end (state 3bz::deflate-state))
  (flet ((output (output-start output-end)
           (format T "~&OUTPUT ~4x" (crc32-array (3bz::ds-output-buffer state) output-start output-end))
           (let ((consumed (funcall function (3bz::ds-output-buffer state) output-start output-end)))
             (cond ((< consumed output-end)
                    (setf (deflate-state-last-read state) consumed)
                    (print (list :consumed-part output-start consumed output-end))
                    (return-from call-with-decompressed-buffer (if (3bz:input-underrun state) end start)))
                   (T
                    (when (3bz:output-overflow state)
                      (3bz:replace-output-buffer state (3bz::ds-output-buffer state)))
                    (print (list :consumed-full output-start consumed output-end))
                    (setf (deflate-state-last-read state) 0)
                    (setf (deflate-state-available state) (3bz:decompress (deflate-state-input-state state) state)))))))
    (format T "~&CALL ~4x" (crc32-array input start end))
    (when (or (null (deflate-state-input-state state))
              (3bz:input-underrun state))
      (print (list :updating-input start end))
      (setf (deflate-state-input-state state) (3bz:make-octet-vector-context input :start start :end end))
      (setf (deflate-state-last-read state) 0)
      (setf (deflate-state-available state) (3bz:decompress (deflate-state-input-state state) state)))
    (loop (cond ((< (deflate-state-last-read state) (deflate-state-available state))
                 (output (deflate-state-last-read state) (deflate-state-available state)))
                ((3bz:finished state)
                 (print (list :finish end))
                 (return end))
                ((3bz:input-underrun state)
                 (print (list :underrun end))
                 (return end))
                (T
                 (print (list :retry start))
                 (return start))))))

(defmethod make-compression-state ((format (eql NIL)) &key buffer)
  NIL)

(defmethod make-compression-state ((format (eql :store)) &key buffer)
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
