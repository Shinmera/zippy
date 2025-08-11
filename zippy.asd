(asdf:defsystem zippy
  :version "1.1.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A fast zip archive library"
  :homepage "https://shinmera.com/docs/zippy"
  :bug-tracker "https://shinmera.com/project/zippy/issues"
  :source-control (:git "https://shinmera.com/project/zippy.git")
  :serial T
  :components ((:file "package")
               (:file "conditions")
               (:file "toolkit")
               (:file "parser")
               (:file "io")
               (:file "tables")
               (:file "compression")
               (:file "encryption")
               (:file "pkware-encryption")
               (:file "structures")
               (:file "zippy")
               (:file "decode")
               (:file "encode")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :file-attributes
               :pathname-utils
               :filesystem-utils
               :alexandria
               :nibbles
               :babel
               :3bz
               :salza2))
