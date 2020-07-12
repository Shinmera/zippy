#|
 This file is a part of zippy
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.zippy)

(defvar *structures* (make-hash-table :test 'eql))

(defun decode-structure (vector index)
  (let* ((signature (nibbles:ub32ref/le vector index))
         (parser (or (gethash signature *structures*)
                     (error "Don't know how to process block with signature~%  ~x"
                            signature))))
    (funcall (first parser) vector (+ index 4))))

(defun read-structure (stream)
  (let* ((signature (nibbles:read-ub32/le stream))
         (parser (or (gethash signature *structures*)
                     (error "Don't know how to process block with signature~%  ~x"
                            signature))))
    (funcall (second parser) stream)))

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

(defun binary-type-writer (type)
  (ecase type
    (ub8 'write-byte)
    (ub16 'nibbles:write-ub16/le)
    (ub32 'nibbles:write-ub32/le)
    (ub64 'nibbles:write-ub64/le)
    (character 'write-byte)))

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
                       (incf ,index ,(binary-type-size type)))))
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
                    do (setf (aref ,name i) (,(binary-type-reader type) ,stream)))))
          (T
           `(let ((,name (,(binary-type-reader type) ,stream))))))))

(defun generate-record-writer (record stream)
  (destructuring-bind (name type &optional count) record
    (cond ((typep type 'integer)
           (let ((btype (integer-binary-type type)))
             `(,(binary-type-writer btype) ,type ,stream)))
          (count
           (if (eql type 'character)
               `(let ((vec (babel:string-to-octets ,name :encoding :utf-8)))
                  (loop for char across vec
                        do (write-byte char ,stream)))
               `(loop for i from 0 below ,count
                      do (,(binary-type-writer type) (aref ,name i) ,stream))))
          (T
           `(,(binary-type-writer type) ,name ,stream)))))

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
  (destructuring-bind (name signature) (if (listp name) name (list name NIL))
    (let ((fields (mapcar #'first records))
          (constructor (intern (format NIL "~a-~a" 'make name)))
          (decode-name (intern (format NIL "~a-~a" 'decode name)))
          (read-name (intern (format NIL "~a-~a" 'read name)))
          (write-name (intern (format NIL "~a-~a" 'write name))))
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
             (,constructor ,@fields)))
         (defun ,write-name (structure stream)
           ,@(when signature
               `((nibbles:write-ub32/le ,signature stream)))
           (with-nesting
             (with-slots ,fields structure
               ,@(loop for record in records
                       collect (generate-record-writer record 'stream)))))
         ,@(when signature
             `((setf (gethash ,signature *structures*)
                     (list #',decode-name #',read-name #',write-name))))))))

(defun decode-msdos-timestamp (date time)
  (let ((yy (ldb (byte 7 9) date))
        (mm (ldb (byte 4 5) date))
        (dd (ldb (byte 5 0) date))
        (h (ldb (byte 5 11) time))
        (m (ldb (byte 6 5) time))
        (s (ldb (byte 5 0) time)))
    (encode-universal-time (* 2 s) m h dd mm (+ 1980 yy))))

(defun encode-msdos-timestamp (timestamp)
  (multiple-value-bind (s m h dd mm yy) (decode-universal-time timestamp)
    (let ((date 0)
          (time 0))
      (setf (ldb (byte 7 9) date) (- yy 1980))
      (setf (ldb (byte 4 5) date) mm)
      (setf (ldb (byte 5 0) date) dd)
      (setf (ldb (byte 5 11) time) h)
      (setf (ldb (byte 6 5) time) m)
      (setf (ldb (byte 5 0) time) (floor s 2))
      (values date time))))

(defun decode-string (octets flags)
  (babel:octets-to-string octets :encoding (if (logbitp 11 flags) :utf-8 :cp437)))

(defun decode-version (version)
  (multiple-value-bind (major minor) (floor (ldb (byte 8 0) version) 10)
    (list major minor)))
