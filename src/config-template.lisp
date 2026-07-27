;; add urls to search in priority order
(repocicl:search "https://github.com/fukamachi/qlot")
(repocicl:search "https://github.com/fukamachi/")
(repocicl:search "https://github.com/metarepl/")
(repocicl:search "https://github.com/fosskers")

;; add systems to ensure are cloned
(repocicl:clone :metarepl.base.click)
(repocicl:clone :filepaths)

;; add systems to ensure are cloned and latest
(repocicl:fetch :mito)
(repocicl:fetch :qlot)

;; specify ref andor type andor url
(repocicl:clone :metarepl.base.click :repo-type :git :ref "hA5h")
(repocicl:fetch :metarepl.base.click :ref "hA5h")
(repocicl:clone :filepaths :url "https://github.com/fosskers")
