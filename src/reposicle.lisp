(in-package :reposicle)

(defvar *search-addresses* nil)
(defvar *cloned-list* nil)   ; list of URLs to ensure-cloned
(defvar *updated-list* nil)  ; list of URLs (or (url :ref "xxx")) to ensure-updated

(defvar *offline-mode* (uiop:getenv "REPOSICLE_OFFLINE"))
(defvar *github-token* (uiop:getenv "REPOSICLE_GITHUB_TOKEN"))

(defvar *config-path*
  (merge-pathnames ".config/reposicle/config.lisp" (user-homedir-pathname)))

(defvar *reposicle-dir* (merge-pathnames "reposicle/" (user-homedir-pathname)))
(defvar *common-lisp-dir* (merge-pathnames "common-lisp/" (user-homedir-pathname)))

;; ----------------------------------------------------------------------
;; Low-level helpers (unchanged from previous solid version)
;; ----------------------------------------------------------------------

(defun ensure-dirs ()
  (ensure-directories-exist *reposicle-dir*)
  (ensure-directories-exist *common-lisp-dir*))

(defun repo-name-from-url (url)
  (let* ((trimmed (string-right-trim "/" url))
         (name (car (last (split-string trimmed :separator '(#\/))))))
    (if (ends-with-subseq ".git" name)
        (subseq name 0 (- (length name) 4))
        name)))

(defun local-repo-path (url)
  (merge-pathnames (repo-name-from-url url) *reposicle-dir*))

(defun symlink-path (repo-name)
  (merge-pathnames repo-name *common-lisp-dir*))

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
         (error "Reposicle: ~A exists and is not our symlink. Refusing to overwrite." link)))
      (t
       (format t "~&Reposicle: Symlinking ~A -> ~A~%" link target)
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
        (warn "Reposicle [offline]: Skipping clone ~A" url)
        (return-from git-clone nil))
      (format t "~&Reposicle: Cloning ~A~%" url)
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
      (format t "~&Reposicle: Updating ~A~%" url)
      (with-current-directory (target)
        (run-program '("git" "pull" "--ff-only" "--depth" "1")
                     :output t :error-output :output :ignore-error-status t)
        (when ref
          (run-program `("git" "checkout" ,ref)
                       :output nil :error-output :output))))))

;; ----------------------------------------------------------------------
;; Config loading
;; ----------------------------------------------------------------------

(defun load-config (path &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (when (probe-file path)
    (format t "~&Reposicle: Loading config ~A~%" path)
    (let ((*package* (find-package :reposicle)))
      (load path))))

(defun add-search (address)
  (pushnew (string-right-trim "/" address) *search-addresses* :test #'string-equal))

(defun ensure-cloned (url &key ref)
  (pushnew (if ref (list url :ref ref) url) *cloned-list* :test #'equal))

(defun ensure-updated (url &key ref)
  (pushnew (if ref (list url :ref ref) url) *updated-list* :test #'equal))

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
              (format t "~&Reposicle: GitHub discovered repo ~A for system ~S~%" repo-url system-name)
              (concatenate 'string repo-url ".git"))))
      (error (e)
        (warn "Reposicle: GitHub search error for ~A: ~A" system-name e)
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
                  (format t "~&Reposicle: Discovered and linked system ~S~%" sys)
                  (return-from strategy-remote-discovery asd))))))))
    nil))

;; ----------------------------------------------------------------------
;; Main ASDF entry point (exact order you specified)
;; ----------------------------------------------------------------------

(defun find-system-via-reposicle (system-name)
  "Entry point — follows your exact strategy order."
  (format t "~&Reposicle: Triggered for system ~S~%" system-name)

  ;; 1. Refresh config
  (when (probe-file *config-path*)
    (load-config *config-path*))

  ;; 2. Strategy: Ensure cloned
  (strategy-ensure-cloned)

  ;; 3. Strategy: Ensure updated
  (strategy-ensure-updated)

  ;; Re-scan ASDF after possible cloning (keeps registry current)
  (asdf:clear-configuration)
  (asdf:initialize-source-registry)

  ;; 4. Strategy: Native ASDF discovery (local + symlinks)
  (let ((found (strategy-asdf-discovery system-name)))
    (when found
      (return-from find-system-via-reposicle found)))

  ;; 5. Final Strategy: Remote discovery
  (strategy-remote-discovery system-name))

;; ----------------------------------------------------------------------
;; Setup
;; ----------------------------------------------------------------------

(defun setup ()
  (ensure-dirs)
  (when (probe-file *config-path*)
    (load-config *config-path*))
  ;; Register at the very end of the search list
  (unless (member 'find-system-via-reposicle asdf:*system-definition-search-functions*)
    (appendf asdf:*system-definition-search-functions* '(find-system-via-reposicle)))
  (format t "~&Reposicle: Setup complete (~D search addresses).~%"
          (length *search-addresses*)))

(eval-when (:load-toplevel :execute)
  (setup))
