
(defpackage #:repocicl
  (:use #:cl #:uiop)
  (:export #:source
           #:clone
           #:update

           #:config-load
           #:config-show
           #:config-clear

           #:*source-urls*
           #:*clone-systems*
           #:*update-systems*

           #:system-list

           #:*config-path*
           #:*download*
           #:*verbose*
           #:*local-only*
           #:*github-token*))
