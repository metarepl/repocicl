;; add systems to ensure are cloned
(clone :metarepl.base.click)
(clone :filepaths)

;; add systems to ensure are cloned and latest
(update :mito)
(update :qlot)

;; add urls to search in priority order
(source "https://github.com/fukamachi/qlot")
(source "https://github.com/fukamachi/")
(source "https://github.com/metarepl/")
(source "https://github.com/fosskers")

;; specify ref andor type andor url
;; (clone :metarepl.base.click :repo-type :git :ref "hA5h")
;; (update :metarepl.base.click :ref "hA5h")
;; (clone :filepaths :url "https://github.com/fosskers")
