#|
 This file is a part of zippy
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.zippy)

(defgeneric call-with-decrypted-buffer (function input length state))
(defgeneric call-with-encrypted-buffer (function input length state))
(defgeneric make-decryption-state (format input password &key buffer))

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
