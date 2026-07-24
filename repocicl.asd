(defsystem "metarepl.systems.repocicl"
  :description "ocicl for repositories"
  :author "metarepl (https://github.com/metarepl)"
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
               (:module "src"
                :components ((:file "repocicl")))
               (:module "test"
                :components ((:file "test")))))
