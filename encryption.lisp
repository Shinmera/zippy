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

(defmethod make-decryption-state ((format (eql :pkware)) (input stream) password &key buffer)
  (let ((initial-state (make-array 12 :element-type '(unsigned-byte 8))))
    (read-sequence initial-state input)
    (make-pkware-decrypt-state (ensure-buffer buffer) (ensure-password password) initial-state 0)))

(defmethod make-decryption-state ((format (eql :pkware)) (input vector-input) password &key buffer)
  (let ((state (make-pkware-decrypt-state (ensure-buffer buffer) (ensure-password password)
                                          (vector-input-vector input) (vector-input-index input))))
    (incf (vector-input-index input) 12)
    state))

(defmethod call-with-decrypted-buffer (function (input stream) length (state pkware-decrypt-state))
  (loop with buffer = (pkware-decrypt-state-buffer state)
        while (< 0 length)
        for read = (read-sequence buffer input)
        do (loop for i from 0 below read
                 for byte = (aref buffer i)
                 for decrypted = (logand #xFF (logxor byte (pkware-decrypt-byte state)))
                 do (update-pkware-state state decrypted)
                    (setf (aref buffer i) decrypted))
           (decf length read)
           (funcall function buffer 0 read)))

(defmethod call-with-decrypted-buffer (function (input vector-input) length (state pkware-decrypt-state))
  (loop with inbuffer = (vector-input-vector input)
        with index = (vector-input-index input)
        with buffer = (pkware-decrypt-state-buffer state)
        for read = (min length (length buffer))
        while (< 0 length)
        do (loop for i from 0 below read
                 for byte = (aref inbuffer index)
                 for decrypted = (logxor byte (pkware-decrypt-byte state))
                 do (update-pkware-state state decrypted)
                    (setf (aref buffer i) decrypted)
                    (incf index))
           (decf length read)
           (funcall function buffer 0 read)))
