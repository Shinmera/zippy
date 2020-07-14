#|
 This file is a part of zippy
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.zippy)

(defstruct (vector-input (:constructor make-vector-input (vector index)))
  (vector NIL :type (vector (unsigned-byte 8) *) :read-only T)
  (index 0 :type fixnum))

(defun seek (input target)
  (etypecase input
    (vector-input
     (setf (vector-input-index input) target))
    (stream
     (file-position input target))))

(defun has-more (input)
  (etypecase input
    (vector-input
     (< (vector-input-index input) (length (vector-input-vector input))))
    (stream
     (< (file-position input) (file-length input)))))

(defun index (input)
  (etypecase input
    (vector-input
     (vector-input-index input))
    (stream
     (file-position input))))

(defmethod size ((input vector-input))
  (length (vector-input-vector input)))

(defmethod size ((input stream))
  (file-length input))

(defun ub32 (input)
  (etypecase input
    (vector-input
     (prog1 (nibbles:ub32ref/le (vector-input-vector input) (vector-input-index input))
       (incf (vector-input-index input) 4)))
    (stream
     (nibbles:read-ub32/le input))))

(defun output (output array start end)
  (etypecase output
    (vector-input
     (loop with vector = (vector-input-vector output)
           for i from start below end
           for j from (vector-input-index output)
           do (setf (aref vector j) (aref array i)))
     (incf (vector-input-index output) (- end start)))
    (stream
     (write-sequence array output :start start :end end))))

(defun parse-structure* (input)
  (etypecase input
    (vector-input
     (multiple-value-bind (value index)
         (decode-structure (vector-input-vector input) (vector-input-index input))
       (setf (vector-input-index input) index)
       value))
    (stream
     (read-structure input))))

(defun write-structure* (structure input)
  (etypecase input
    (vector-input
     (setf (vector-input-index input)
           (encode-structure structure (vector-input-vector input) (vector-input-index input))))
    (stream
     (write-structure structure input)))
  input)

(defmacro parse-structure (structure-type input-var)
  (let ((input (gensym "INPUT")))
    `(let ((,input ,input-var))
       (etypecase ,input
         (vector-input
          (multiple-value-bind (value index)
              (,(intern (format NIL "~a-~a" 'decode structure-type))
               (vector-input-vector ,input) (vector-input-index ,input))
            (setf (vector-input-index ,input) index)
            value))
         (stream
          (,(intern (format NIL "~a-~a" 'read structure-type)) ,input))))))
