#|
 This file is a part of zippy
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.zippy)

(defun integer-binary-type (integer)
  (cond ((<= integer #xFF) 'ub8)
        ((<= integer #xFFFF) 'ub16)
        ((<= integer #xFFFFFFFF) 'ub32)
        ((<= integer #xFFFFFFFFFFFFFFFF) 'ub64)
        (T (error "Integer too large."))))

(defun binary-type-size (type)
  (ecase type
    (ub8 1)
    (ub16 2)
    (ub32 4)
    (ub64 8)
    (character 1)))

(defun binary-type-type (type)
  (ecase type
    (ub8 '(unsigned-byte 8))
    (ub16 '(unsigned-byte 16))
    (ub32 '(unsigned-byte 32))
    (ub64 '(unsigned-byte 64))
    (character '(unsigned-byte 8))))

(defun binary-type-reader (type)
  (ecase type
    (ub8 'aref)
    (ub16 'nibbles:ub16ref/le)
    (ub32 'nibbles:ub32ref/le)
    (ub64 'nibbles:ub64ref/le)
    (character 'aref)))

(defun generate-record-reader (record vector index)
  (destructuring-bind (name type &optional count) record
    (cond ((typep type 'integer)
           (let ((btype (integer-binary-type type)))
             `(let ((,name (,(binary-type-reader btype) ,vector ,index)))
                (incf ,index ,(binary-type-size btype))
                (unless (= ,type ,name)
                  (error "Record does not match signature.")))))
          (count
           `(let ((,name (make-array ,count :element-type ',(binary-type-type type))))
              (loop for i from 0 below (length ,name)
                    do (setf (aref ,name i) (,(binary-type-reader type) ,vector ,index))
                       (incf ,index ,(binary-type-size type)))
              ,@(when (eql type 'character)
                  ;; FIXME: Not quite right -- only when GPB 11 is set.
                  `((setf ,name (babel:octets-to-string ,name :encoding :utf-8))))))
          (T
           `(let ((,name (,(binary-type-reader type) ,vector ,index)))
              (incf ,index ,(binary-type-size type)))))))

(defmacro with-nesting (&body forms)
  (cond ((null forms)
         NIL)
        ((null (cdr forms))
         (first forms))
        (T
         (append (first forms) `((with-nesting ,@(rest forms)))))))

(defmacro define-byte-structure (name &body records)
  (let ((fields (mapcar #'first records))
        (constructor (intern (format NIL "~a-~a" 'make name)))
        (read-name (intern (format NIL "~a-~a" 'decode name))))
    `(progn
       (defstruct (,name (:constructor ,constructor ,fields))
         ,@fields)
       (defun ,read-name (vector index)
         (with-nesting
             ,@(loop for record in records
                     collect (generate-record-reader record 'vector 'index))
           (,constructor ,@fields))))))
