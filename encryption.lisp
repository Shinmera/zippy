#|
 This file is a part of zippy
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.zippy)

(defgeneric make-decryption-state (format input password &key buffer &allow-other-keys))
(defgeneric call-with-decrypted-buffer (function input length state))

(defgeneric make-encryption-state (format password &key buffer))
(defgeneric call-with-encrypted-buffer (function vector start end state))

(defmethod make-decryption-state (format input password &rest args)
  (declare (ignore args))
  (error "Unsupported encryption method: ~a" format))

(defmethod make-decryption-state ((format (eql NIL)) (input vector-input) password &key buffer)
  input)

(defmethod make-decryption-state ((format (eql NIL)) (input stream) password &key buffer)
  (ensure-buffer buffer))

(defmethod call-with-decrypted-buffer (function (input stream) length (buffer vector))
  (loop while (< 0 length)
        for read = (read-sequence buffer input :end (min (length buffer) length))
        do (funcall function buffer 0 read)
           (decf length read)))

(defmethod call-with-decrypted-buffer (function (input vector-input) length (format vector))
  (let ((start (vector-input-index input)))
    (funcall function (vector-input-vector input) start (+ start length))))

(defmethod make-encryption-state ((format (eql NIL)) password &key buffer)
  NIL)

(defmethod call-with-encrypted-buffer (function vector start end (state (eql NIL)))
  (funcall function vector start end))

;; TODO: Support for AE-X https://www.winzip.com/win/en/aes_info.html
;; TODO: Support for other encryption methods
;; TODO: Support for central directory encryption
