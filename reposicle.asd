(defsystem "lamb.files.payload"
  :description "file hash compres and send utility"
  :author "common-lamb (https://github.com/common-lamb)"
  :version "0.0.1"
  :license "MIT"
  :depends-on (
               :cmd
               :str
               :cl-ppcre
               :chipz
               :alexandria
               :journal
               :local-time
               :filepaths
               :filesystem-utils
               )
  :serial t
  :components ((:file "package")
               (:file "payload")))
