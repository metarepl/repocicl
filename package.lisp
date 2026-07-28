
(defpackage #:repocicl
  (:use #:cl #:uiop)
  (:export #:source
           #:clone
           #:fetch

           #:config-load
           #:config-show
           #:config-clear

           #:*source-urls*
           #:*clone-systems*
           #:*fetch-systems*

           #:system-list

           #:*config-path*
           #:*download*
           #:*verbose*
           #:*local-only*
           #:*github-token*))
