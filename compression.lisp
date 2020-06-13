#|
 This file is a part of zippy
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.zippy)

(defgeneric decompress-vector (vector index length format))
(defgeneric decompress-stream (stream length format))
(defgeneric compress-vector (vector out-stream format))
(defgeneric compress-stream (in-stream out-stream format))

(defmethod decompress-vector (vector index length (format (eql :deflate)))
  (3bz:decompress-vector vector :format :deflate :start index :end (+ index length)))
