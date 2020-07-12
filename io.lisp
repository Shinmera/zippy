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

(defun parse-structure* (input)
  (etypecase input
    (vector-input
     (multiple-value-bind (value index)
         (decode-structure (vector-input-vector input) (vector-input-index input))
       (setf (vector-input-index input) index)
       value))
    (stream
     (read-structure input))))

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

(defun call-with-io (function io &key (direction :input) (if-exists :error) (index 0))
  (etypecase io
    ((or pathname string)
     (handler-bind ((archive-file-required
                      (lambda (c)
                        (let ((id (slot-value c 'id)))
                          ;; FIXME: Leaking FDs
                          (use-value (open (make-pathname :type (format NIL "z~2,'0d" (1+ id)) :defaults io)
                                           :element-type '(unsigned-byte 8)))))))
       (with-open-file (stream io :direction direction :if-exists if-exists :element-type '(unsigned-byte 8))
         (funcall function stream))))
    (stream
     (funcall function io))
    (vector
     (funcall function (make-vector-input io index)))))

(defmacro with-io ((io target &rest args) &body body)
  `(call-with-io (lambda (,io) ,@body) ,target ,@args))
