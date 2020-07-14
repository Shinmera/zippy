#|
 This file is a part of zippy
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.zippy)

(defgeneric call-with-decrypted-buffer (function input length format &key buffer))
(defgeneric call-with-encrypted-buffer (function input length format &key buffer))

