(in-package :repocicl)

(defvar *config-path*
  (merge-pathnames "repocicl/config.lisp" (uiop:xdg-config-home)))
(defvar *repocicl-dir* (merge-pathnames "repocicl/" (user-homedir-pathname)))

(defvar *common-lisp-dir* (merge-pathnames "common-lisp/" (user-homedir-pathname)))

(defvar *offline-mode* (uiop:getenv "REPOCICL_OFFLINE"))
(defvar *github-token* (uiop:getenv "REPOCICL_GITHUB_TOKEN"))
(defvar *verbose* nil)

(defvar *search-addresses* nil) ; list of github urls to search for systems below
(defvar *clone-list* nil)   ; list of URLs to ensure-cloned
(defvar *fetch-list* nil)  ; list of URLs (or (url :ref "xxx")) to ensure-updated

;; ----------------------------------------------------------------------
;; Low-level helpers
;; ----------------------------------------------------------------------

;; &&& test that ocicl is available goes into setup
;; we assume ocicl has been setup and loaded successfuly before reposicle
;; for compatibility and a potential future merge we will copy some internal functions of ocicl

(defun split-on-delimiter (line delim)
"replace ocicl"
(str:split delim line))

(defun should-log ()
"copy ocicl
Whether or not OCICL should output useful log info to *VERBOSE*."
(and *verbose* (or (eq t *verbose*) (output-stream-p *verbose*))))

(defun sanitize-system-name (name)
  " copy ocicl
Sanitize system name to prevent command injection."
  (let ((name-str (princ-to-string name)))
    ;; Only allow alphanumeric, dash, underscore, dot, plus, and slash
    (if (every (lambda (c) (or (alphanumericp c) (find c "-_.+/"))) name-str)
        name-str
(error "Invalid system name: ~A" name-str))))

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
  (org.shirakumo.filesystem-utils:symbolic-link-p path))

(defun symbolic-link-broken-p (path)
  (and (org.shirakumo.filesystem-utils:symbolic-link-p path)
       (let* (;; unresolved namestring will be link
              (components (org.shirakumo.pathname-utils:components path))
              ;; when target present, resolved namestring will be target
              ;; when target absent, resolved namestring will be link
              (components-resolved (org.shirakumo.pathname-utils:components
                 (uiop:resolve-symlinks path)))
              ;; grab the variant field from the components
              ;; tests with :directory were the only other option
              (namestring (getf components :namestring))
              (namestring-resolved (getf components-resolved :namestring))
              ;; clean up, ensure ~ is converted to /home/user
              ;; without any other path resolution
              (parsedstring (pathname-utils:native-namestring
                             namestring))
              (parsedstring-resolved (pathname-utils:native-namestring
                                      namestring-resolved)))
         ;; namestring resolves to the symlink file
         ;; only when target not found
         (string= parsedstring parsedstring-resolved))))

(defun safe-delete-broken-symlink (path)
  (when (and (symbolic-link-p path)
             (symbolic-link-broken-p path))
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

(defun system-list (&key (shadowed nil))
;; &&& copy me
"Return list of all known system names from *repocicl-dir*
filtered by those which are not shadowed, or show shadowed"
(initialize-globals)
(append (when *local-ocicl-systems*
(loop for key being the hash-keys of *local-ocicl-systems*
collect key))
(when *global-ocicl-systems*
(loop for key being the hash-keys of *global-ocicl-systems*
collect key))))

;; ----------------------------------------------------------------------
;; git helpers
;; ----------------------------------------------------------------------

;;;; update system

;; &&& follow clone system

;;;; clone system

(defun clone-repo-run (program-args url directory)
  (uiop:run-program `(,@program-args ,url)
                    :directory directory
                    :output :interactive
                    :error-output :interactive))

(defmethod clone-repo (url directory (repository-type (eql :git)))
  (clone-repo-run '("git" "clone") url directory))

(defmethod clone-repo (url directory (repository-type (eql :svn)))
  (clone-repo-run '("svn" "co") url directory))

(defmethod clone-repo (url directory (repository-type (eql :darcs)))
  (clone-repo-run '("darcs" "get") url directory))

(defmethod clone-repo (url directory (repository-type (eql :hg)))
  (clone-repo-run '("hg" "clone") url directory))

(defgeneric clone-repo (url directory &optional (repository-type :git))
  (:documentation "clone a repository of REPOSITORY-TYPE from URL, as a
subdirectory of DIRECTORY. REPOSITORY-TYPE can be :GIT, :SVN, :DARCS, or :HG"))

(defun clone-system (url
                     &key (repository-type :git) (repocicl-dir *repocicl-dir*))
  "Download the ASDF-SYSTEM of REPOSITORY-TYPE from the URL"
  (let ((directory (merge-pathnames
                           (make-pathname :directory &&&) ;; from url
                           repocicl-dir)))
    (clone-repo url directory repository-type)))

;;;; check and act
(defun system-available-p (asdf-system)
  "check if download is needed"
  (asdf:find-system asdf-system nil))

(defun make-system-available (asdf-system)
  "make system known to asdf"
  (asdf:clear-configuration)
  (asdf:find-system asdf-system))

(defun add-system (asdf-system &&&)
  (when (not (system-available-p asdf-system))
    (progn
      (clone-system asdf-system)
      (make-system-available asdf-system))))
(defun git-clone (url &key ref)
  "slop vesion"
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
  "slop version"
  (let ((target (local-repo-path url)))
    (when (and (directory-exists-p target) (not *offline-mode*))
      (format t "~&Repocicl: Updating ~A~%" url)
      (with-current-directory (target)
        (run-program '("git" "pull" "--ff-only" "--depth" "1")
                     :output t :error-output :output :ignore-error-status t)
        (when ref
          (run-program `("git" "checkout" ,ref)
                       :output nil :error-output :output))))))

(defun repocicl-update (name)
  "Install system NAME using the ocicl command.
;; &&& copy of ocicl-install
"
  (let* ((safe-name (sanitize-system-name name))
         (cmd `("ocicl" ,@(when *verbose* '("-v"))
                        ,@(when *force-global* '("--global"))
                        "install"
                        ,safe-name)))
    (when (should-log)
      (format *verbose* "; running: ~A~%" cmd))
    (handler-case
        (uiop:run-program cmd
                          :output (or *verbose* '(:string))
                          :error-output *error-output*)
      (error (e)
        (when (should-log)
          (format *verbose* "; Error installing ~A: ~A~%" safe-name e))
        (error e))))
  (setf *local-systems-csv-timestamp* 0))

(defun repocicl-clone (name)
  "Install system NAME using the ocicl command.
;; &&& copy of ocicl-install "
  (let* ((safe-name (sanitize-system-name name))
         (cmd `("ocicl" ,@(when *verbose* '("-v"))
                        ,@(when *force-global* '("--global"))
                        "install"
                        ,safe-name)))
    (when (should-log)
      (format *verbose* "; running: ~A~%" cmd))
    (handler-case
        (uiop:run-program cmd
                          :output (or *verbose* '(:string))
                          :error-output *error-output*)
      (error (e)
        (when (should-log)
          (format *verbose* "; Error installing ~A: ~A~%" safe-name e))
        (error e))))
  (setf *local-systems-csv-timestamp* 0))

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

;; ----------------------------------------------------------------------
;; Strategies
;; ----------------------------------------------------------------------

(defun strategy-ensure-cloned ()
  "Process *clone-list*"
  (dolist (entry *clone-list*)
    (destructuring-bind (url &key ref) (if (consp entry) entry (list entry))
      (git-clone url :ref ref))))

(defun strategy-ensure-updated ()
  "Process *fetch-list*"
  (dolist (entry *fetch-list*)
    (destructuring-bind (url &key ref) (if (consp entry) entry (list entry))
      (git-clone url :ref ref)   ; ensure present first
      (git-update url :ref ref))))

(defun strategy-asdf-discovery (system-name)
  "Let ASDF's normal mechanisms (including our symlinks in ~/common-lisp/) try to find the system.
   This keeps local discovery current after cloning."
  (asdf:search-for-system-definition system-name))

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

(defun find-asdf-system-file (name download-p)
  ;; &&& copy me
  "Find ASDF system file for NAME, optionally downloading if DOWNLOAD-P is true."
  (initialize-globals)
  (labels ((try-load (systems systems-dir)
             (let ((match (and systems (gethash (mangle name) systems)))) ; lint:suppress
               (when match
                   (let ((pn (merge-pathnames (rest match) systems-dir)))
                     (when (should-log)
                       (format *verbose* "; checking for ~A: " pn))
                     (if (uiop:file-exists-p pn)
                         (progn
                           (when (should-log) (format *verbose* "found~%"))
                           pn)
                         (when (should-log) (format *verbose* "missing~%"))))))))
    (or (try-load *local-ocicl-systems* *local-systems-dir*)
        (unless *local-only*
          (try-load *global-ocicl-systems* *global-systems-dir*))
        (when download-p
          (ocicl-install name)
          (setf *local-ocicl-systems* (read-systems-csv *local-systems-csv*))
          (find-asdf-system-file name nil)))))

(defun system-definition-searcher (name)
  ;; &&& copy me
  "Search for ASDF system definition file for NAME, using repocicl if needed."
  (unless (or (starts-with-p "asdf/" name) (string= "asdf" name) (string= "uiop" name))
    (let* ((*verbose* (or *verbose* asdf:*verbose-out*))
           (system-file (find-asdf-system-file name *download*)))
      (when (and system-file
                 (string= (pathname-name system-file) name))
        system-file))))

(defun system-definition-searcher (system-name)
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
;; Config loading
;; ----------------------------------------------------------------------

;; demo config
;; &&& borrow format from qlot and cl-autorepo

(defun load-config (path)
  (when (probe-file path)
    (format t "~&Repocicl: Loading config ~A~%" path)
    ;; &&& exclude any unapproved operations
    ;; sanitize system names
    ;; url qc etc &&&
    (load path)))

(defun search (url)
  (pushnew (string-right-trim "/" url) *search-addresses* :test #'string-equal))

(defun clone (asdf-system &key type ref url)
  (pushnew (if ref (list url :ref ref) url) *clone-list* :test #'equal))

(defun fetch (asdf-system &key type ref url)
  (pushnew (if ref (list url :ref ref) url) *fetch-list* :test #'equal))

(defun show-config ()
  "&&& not implemented")

(defun clear-config ()
  "&&& not implemented")

;; &&& move me
;; &&& because search only knows github
;; &&& if :type is not :git then url must be provided

;; ----------------------------------------------------------------------
;; Setup
;; ----------------------------------------------------------------------

(defun initialize-globals ()
  ;;  &&& copy me
  "Initialize global variables for local and global system directories and CSV files."
  (unless *local-systems-dir*
    (let ((workdir (find-workdir (uiop:getcwd))))
      (setf *local-systems-dir* (merge-pathnames *relative-systems-dir* workdir))
      (setf *local-systems-csv* (merge-pathnames *systems-csv* workdir))))

  (unless (or *local-only* *global-systems-dir*)
    (let* ((config-file (merge-pathnames "ocicl-globaldir.cfg" (get-ocicl-dir)))
           (globaldir (if (uiop:file-exists-p config-file)
                          (handler-case
                              (uiop:ensure-absolute-pathname (uiop:read-file-line config-file))
                            (error (e)
                              (when (should-log)
                                (format *verbose* "; Error reading config file ~A: ~A~%" config-file e))
                              (get-ocicl-dir)))
                          (get-ocicl-dir))))

      (setf *global-systems-dir* (merge-pathnames *relative-systems-dir* globaldir))
      (setf *global-systems-csv* (merge-pathnames *systems-csv* globaldir))))

  (when (uiop:file-exists-p *local-systems-csv*)
    (let ((timestamp (file-write-date *local-systems-csv*)))
      (when (> timestamp *local-systems-csv-timestamp*)
        (handler-case
            (progn
              (setf *local-ocicl-systems* (read-systems-csv *local-systems-csv*))
              (setf *local-systems-csv-timestamp* timestamp))
          (error (e)
            (when (should-log)
              (format *verbose* "; Error reading local systems CSV ~A: ~A~%" *local-systems-csv* e)))))))

  (when (and (not *local-only*) (uiop:file-exists-p *global-systems-csv*))
    (let ((timestamp (file-write-date *global-systems-csv*)))
      (when (> timestamp *global-systems-csv-timestamp*)
        (handler-case
            (progn
              (setf *global-ocicl-systems* (read-systems-csv *global-systems-csv*))
              (setf *global-systems-csv-timestamp* timestamp))
          (error (e)
            (when (should-log)
              (format *verbose* "; Error reading global systems CSV ~A: ~A~%" *global-systems-csv* e))))))))

(defun ensure-dirs ()
  (ensure-directories-exist *repocicl-dir*)
  (ensure-directories-exist *common-lisp-dir*))

(defun setup ()

  (initialize-globals)
  (ensure-dirs)

  (when (probe-file *config-path*)
    (load-config *config-path*))

  ;; Register at the very end of the search list
  (unless (member 'repocicl:find-system-via-repocicl
                  asdf:*system-definition-search-functions*)
    (setf asdf:*system-definition-search-functions*
          (append asdf:*system-definition-search-functions*
                  (list 'system-definition-searcher))))

  (pushnew :REPOCICL *features*))

(eval-when (:load-toplevel :execute)
  (setup))
