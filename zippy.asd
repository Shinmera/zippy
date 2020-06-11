#|
 This file is a part of zippy
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem zippy
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A fast zip archive library"
  :homepage "https://github.com/Shinmera/zippy"
  :serial T
  :components ((:file "package")
               (:file "structures")
               (:file "zippy")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :nibbles
               :babel
               :3bz))
