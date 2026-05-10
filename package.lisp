
(defpackage :reposicle
  (:use :cl :uiop)
  (:export #:add-search
           #:ensure-cloned
           #:ensure-updated
           #:setup
           #:*search-addresses*
           #:*offline-mode*
           #:*config-path*
           #:*github-token*))
