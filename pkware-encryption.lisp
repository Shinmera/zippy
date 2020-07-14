#|
 This file is a part of zippy
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.zippy)

(defstruct (pkware-decrypt-state
            (:constructor %make-pkware-decrypt-state (buffer)))
  (buffer NIL :type (simple-array (unsigned-byte 8) (*)))
  (k0 305419896 :type (unsigned-byte 32))
  (k1 591751049 :type (unsigned-byte 32))
  (k2 878082192 :type (unsigned-byte 32)))

(defun crc32-rotate (crc byte)
  (logxor (ldb (byte 24 8) crc)
          (aref 3bz::+crc32/table+ (ldb (byte 8 0) (logxor crc byte)))
          #xFFFFFFFF))

(defun update-pkware-state (state byte)
  (setf (pkware-decrypt-state-k0 state) (crc32-rotate (pkware-decrypt-state-k0 state) byte))
  (setf (pkware-decrypt-state-k1 state) (+ (pkware-decrypt-state-k1 state)
                                           (logand (pkware-decrypt-state-k0 state) #xFF)))
  (setf (pkware-decrypt-state-k1 state) (logand #xFFFFFFFF (1+ (* (pkware-decrypt-state-k1 state) 134775813))))
  (setf (pkware-decrypt-state-k2 state) (crc32-rotate (pkware-decrypt-state-k2 state) (ash (pkware-decrypt-state-k1 state) -24))))

(defun pkware-decrypt-byte (state)
  (let ((temp (logior 2 (pkware-decrypt-state-k2 state))))
    (ash (* temp (logxor temp 1)) -8)))

(defun make-pkware-decrypt-state (buffer password initial-state index)
  (let ((state (%make-pkware-decrypt-state buffer)))
    (loop for byte across password
          do (update-pkware-state state byte))
    (loop for i from index below (+ index 12)
          for byte = (aref initial-state i)
          for c = (logxor byte (pkware-decrypt-byte state))
          do (update-pkware-state state c))
    state))

(defun pkware-decrypt-stream (password stream length)
  (let ((initial-state (make-array 12 :element-type '(unsigned-byte 8)))
        (vector (make-array (- length 12) :element-type '(unsigned-byte 8))))
    (read-sequence initial-state stream)
    (let ((state (make-pkware-decrypt-state password initial-state 0)))
      (loop for i from 0 below (read-sequence vector stream)
            for byte = (aref vector i)
            for decrypted = (logxor byte (pkware-decrypt-byte state))
            do (update-pkware-state state decrypted)
               (setf (aref vector i) decrypted))
      vector)))

(defun pkware-decrypt-vector (password vector start end)
  (let ((state (make-pkware-decrypt-state password vector start)))
    (loop for i from (+ start 12) below end
          for byte = (aref vector i)
          for decrypted = (logxor byte (pkware-decrypt-byte state))
          do (update-pkware-state state decrypted)
             (setf (aref vector i) decrypted))))
