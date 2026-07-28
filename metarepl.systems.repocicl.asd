(defsystem "metarepl.systems.repocicl"
  :description "ocicl for repositories"
  :author "metarepl (https://github.com/metarepl)"
  :version "0.0.1"
  :license "MIT"
  :depends-on (
               :cmd
               :str
               :pathname-utils
               :filesystem-utils
               :quri
               :dexador
               :jsown
               )
  :serial t
  :components ((:file "package")
               (:module "src"
                :components ((:file "repocicl")))
               (:module "test"
                :components ((:file "test")))))
