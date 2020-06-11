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

(defun binary-type-decoder (type)
  (ecase type
    (ub8 'aref)
    (ub16 'nibbles:ub16ref/le)
    (ub32 'nibbles:ub32ref/le)
    (ub64 'nibbles:ub64ref/le)
    (character 'aref)))

(defun binary-type-reader (type)
  (ecase type
    (ub8 'read-byte)
    (ub16 'nibbles:read-ub16/le)
    (ub32 'nibbles:read-ub32/le)
    (ub64 'nibbles:read-ub64/le)
    (character 'read-byte)))

(defun generate-record-decoder (record vector index)
  (destructuring-bind (name type &optional count) record
    (cond ((typep type 'integer)
           (let ((btype (integer-binary-type type)))
             `(let ((,name (,(binary-type-decoder btype) ,vector ,index)))
                (incf ,index ,(binary-type-size btype))
                (unless (= ,type ,name)
                  (error "Record does not match signature.")))))
          (count
           `(let ((,name (make-array ,count :element-type ',(binary-type-type type))))
              (loop for i from 0 below (length ,name)
                    do (setf (aref ,name i) (,(binary-type-decoder type) ,vector ,index))
                       (incf ,index ,(binary-type-size type)))
              ,@(when (eql type 'character)
                  ;; FIXME: Not quite right -- only when GPB 11 is set.
                  `((setf ,name (babel:octets-to-string ,name :encoding :utf-8))))))
          (T
           `(let ((,name (,(binary-type-decoder type) ,vector ,index)))
              (incf ,index ,(binary-type-size type)))))))

(defun generate-record-reader (record stream)
  (destructuring-bind (name type &optional count) record
    (cond ((typep type 'integer)
           (let ((btype (integer-binary-type type)))
             `(let ((,name (,(binary-type-reader btype) ,stream)))
                (unless (= ,type ,name)
                  (error "Record does not match signature.")))))
          (count
           `(let ((,name (make-array ,count :element-type ',(binary-type-type type))))
              (loop for i from 0 below (length ,name)
                    do (setf (aref ,name i) (,(binary-type-reader type) ,stream)))
              ,@(when (eql type 'character)
                  ;; FIXME: Not quite right -- only when GPB 11 is set.
                  `((setf ,name (babel:octets-to-string ,name :encoding :utf-8))))))
          (T
           `(let ((,name (,(binary-type-reader type) ,stream))))))))

(defmacro with-nesting (&body forms)
  (cond ((null forms)
         NIL)
        ((null (cdr forms))
         (first forms))
        (T
         (destructuring-bind (prev . forms) (reverse forms)
           (loop for form in forms
                 for copy = (copy-list form)
                 do (setf (cdr (last copy)) (list prev))
                    (setf prev copy))
           prev))))

(defmacro define-byte-structure (name &body records)
  (let ((fields (mapcar #'first records))
        (constructor (intern (format NIL "~a-~a" 'make name)))
        (decode-name (intern (format NIL "~a-~a" 'decode name)))
        (read-name (intern (format NIL "~a-~a" 'read name))))
    `(progn
       (defstruct (,name (:constructor ,constructor ,fields))
         ,@fields)
       (defun ,decode-name (vector index)
         (with-nesting
             ,@(loop for record in records
                     collect (generate-record-decoder record 'vector 'index))
           (values (,constructor ,@fields) index)))
       (defun ,read-name (stream)
         (with-nesting
           ,@(loop for record in records
                   collect (generate-record-reader record 'stream))
           (,constructor ,@fields))))))
