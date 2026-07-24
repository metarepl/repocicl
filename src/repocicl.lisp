(in-package :repocicl)

(defvar *cloned-list* nil)   ; list of URLs to ensure-cloned
(defvar *updated-list* nil)  ; list of URLs (or (url :ref "xxx")) to ensure-updated
(defvar *search-addresses* nil) ; list of github urls to search for systems below

(defvar *offline-mode* (uiop:getenv "REPOCICL_OFFLINE"))
(defvar *github-token* (uiop:getenv "REPOCICL_GITHUB_TOKEN"))

(defvar *config-path*
  (merge-pathnames "repocicl/config.lisp" (uiop:xdg-config-home)))

(defvar *repocicl-dir* (merge-pathnames "repocicl/" (user-homedir-pathname)))
(defvar *common-lisp-dir* (merge-pathnames "common-lisp/" (user-homedir-pathname)))

;; ----------------------------------------------------------------------
;; Low-level helpers
;; ----------------------------------------------------------------------

(defun ensure-dirs ()
  (ensure-directories-exist *repocicl-dir*)
  (ensure-directories-exist *common-lisp-dir*))

(defun repo-name-from-url (url)
  (let* ((trimmed (string-right-trim "/" url))
         (name (car (last (split-string trimmed :separator '(#\/))))))
    (if (ends-with-subseq ".git" name)
        (subseq name 0 (- (length name) 4))
        name)))

(defun local-repo-path (url)
  (merge-pathnames (repo-name-from-url url) *repocicl-dir*))

(defun symlink-path (asdf-name)
  (merge-pathnames asdf-name *common-lisp-dir*))

(defun symbolic-link-p (path)
  &&&)

(defun safe-delete-broken-symlink (path)
  (when (and (symbolic-link-p path) (not (probe-file path)))
    (delete-file path)))

(defun create-or-update-symlink (target repo-name)
  (let ((link (symlink-path repo-name)))
    (safe-delete-broken-symlink link)
    (cond
      ((probe-file link)
       (unless (and (symbolic-link-p link)
                    (equal (truename link) (truename target)))
         (error "Repocicl: ~A exists and is not our symlink. Refusing to overwrite." link)))
      (t
       (format t "~&Repocicl: Symlinking ~A -> ~A~%" link target)
       (ensure-directories-exist (directory-namestring link))
       (create-symbolic-link target link)))))

(defun find-top-level-asd (repo-dir system-name)
  (let ((sys (string-downcase system-name)))
    (loop for asd in (directory (merge-pathnames "*.asd" repo-dir))
          when (or (string-equal (pathname-name asd) sys)
                   (search sys (namestring asd) :test #'char-equal))
          return asd)))

(defun git-clone (url &key ref)
  (let ((target (local-repo-path url)))
    (unless (directory-exists-p target)
      (when *offline-mode*
        (warn "Repocicl [offline]: Skipping clone ~A" url)
        (return-from git-clone nil))
      (format t "~&Repocicl: Cloning ~A~%" url)
      (run-program `("git" "clone" "--depth" "1" ,url ,target)
                   :output t :error-output :output :ignore-error-status t)
      (when (and ref (directory-exists-p target))
        (with-current-directory (target)
          (run-program `("git" "checkout" ,ref)
                       :output t :error-output :output))))
    (when (directory-exists-p target)
      (create-or-update-symlink target (repo-name-from-url url))
      target)))

(defun git-update (url &key ref)
  (let ((target (local-repo-path url)))
    (when (and (directory-exists-p target) (not *offline-mode*))
      (format t "~&Repocicl: Updating ~A~%" url)
      (with-current-directory (target)
        (run-program '("git" "pull" "--ff-only" "--depth" "1")
                     :output t :error-output :output :ignore-error-status t)
        (when ref
          (run-program `("git" "checkout" ,ref)
                       :output nil :error-output :output))))))

;; ----------------------------------------------------------------------
;; Config loading
;; ----------------------------------------------------------------------

(defun load-config (path)
  (when (probe-file path)
    (format t "~&Repocicl: Loading config ~A~%" path)
    ;; &&& exclude any unapproved operations
    (load path)))

(defun ensure-cloned (url &key ref)
  (pushnew (if ref (list url :ref ref) url) *cloned-list* :test #'equal))

(defun ensure-updated (url &key ref)
  (pushnew (if ref (list url :ref ref) url) *updated-list* :test #'equal))

(defun add-search (url)
  (pushnew (string-right-trim "/" url) *search-addresses* :test #'string-equal))

;; ----------------------------------------------------------------------
;; Strategies
;; ----------------------------------------------------------------------

(defun strategy-ensure-cloned ()
  "Process *cloned-list*"
  (dolist (entry *cloned-list*)
    (destructuring-bind (url &key ref) (if (consp entry) entry (list entry))
      (git-clone url :ref ref))))

(defun strategy-ensure-updated ()
  "Process *updated-list*"
  (dolist (entry *updated-list*)
    (destructuring-bind (url &key ref) (if (consp entry) entry (list entry))
      (git-clone url :ref ref)   ; ensure present first
      (git-update url :ref ref))))

(defun strategy-asdf-discovery (system-name)
  "Let ASDF's normal mechanisms (including our symlinks in ~/common-lisp/) try to find the system.
   This keeps local discovery current after cloning."
  (asdf:search-for-system-definition system-name))

(defun github-code-search (owner system-name)
  "GitHub Code Search for top-level *.asd matching the system name."
  (when *offline-mode* (return-from github-code-search nil))
  (let* ((query (format nil "extension:asd ~A in:path" (string-downcase system-name)))
         (url (format nil "https://api.github.com/search/code?q=~A+user:~A"
                      (quri:url-encode query) owner))
         (headers (when *github-token*
                    `(("Authorization" . ,(format nil "token ~A" *github-token*))
                      ("Accept" . "application/vnd.github.v3+json")))))
    (handler-case
        (let* ((resp (dex:get url :headers headers :connect-timeout 10))
               (data (jsown:parse resp))
               (items (jsown:val data "items")))
          (when items
            (let* ((item (first items))
                   (repo-url (jsown:val (jsown:val item "repository") "html_url")))
              (format t "~&Repocicl: GitHub discovered repo ~A for system ~S~%" repo-url system-name)
              (concatenate 'string repo-url ".git"))))
      (error (e)
        (warn "Repocicl: GitHub search error for ~A: ~A" system-name e)
        nil))))

(defun discover-via-remote (base system-name)
  (let ((owner (car (last (split-string (string-right-trim "/" base) :separator '(#\/))))))
    (or (github-code-search owner system-name)
        ;; fallback
        (concatenate 'string base (string-downcase system-name)))))

(defun strategy-remote-discovery (system-name)
  "Final slow strategy: remote discovery + clone + link."
  (when (or *offline-mode* (null *search-addresses*))
    (return-from strategy-remote-discovery nil))
  (let ((sys (string-downcase system-name)))
    (dolist (base *search-addresses*)
      (let ((repo-url (discover-via-remote base sys)))
        (when repo-url
          (let ((repo-dir (git-clone repo-url)))
            (when repo-dir
              (let ((asd (find-top-level-asd repo-dir sys)))
                (when asd
                  (format t "~&Repocicl: Discovered and linked system ~S~%" sys)
                  (return-from strategy-remote-discovery asd))))))))
    nil))

;; ----------------------------------------------------------------------
;; Main ASDF entry point
;; ----------------------------------------------------------------------

(defun find-system-via-repocicl (system-name)
  "ASDF entry point"
  (format t "~&Repocicl: Triggered for system ~S~%" system-name)
  ;; Refresh config
  (when (probe-file *config-path*)
    (load-config *config-path*))
  ;; Strategy: Ensure cloned
  (strategy-ensure-cloned)
  ;; Strategy: Ensure updated
  (strategy-ensure-updated)
  ;; Re-scan ASDF after possible cloning (keeps registry current)
  (asdf:clear-configuration)
  (asdf:initialize-source-registry)
  ;; Strategy: Native ASDF discovery (local + symlinks)
  (let ((found (strategy-asdf-discovery system-name)))
    (when found
      (return-from find-system-via-repocicl found)))
  ;; Final Strategy: Remote discovery
  (strategy-remote-discovery system-name))

;; ----------------------------------------------------------------------
;; Setup
;; ----------------------------------------------------------------------

(defun setup ()
  (ensure-dirs)
  (when (probe-file *config-path*)
    (load-config *config-path*))
  ;; Register at the very end of the search list
  (unless (member 'find-system-via-repocicl
                  asdf:*system-definition-search-functions*)
    (appendf asdf:*system-definition-search-functions*
             '(find-system-via-repocicl))))

(eval-when (:load-toplevel :execute)
  (setup))
