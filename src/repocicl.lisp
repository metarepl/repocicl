(in-package :repocicl)

(defvar *config-path* nil
  "pathspec to a valid repocicl config file
eg.
(clone :system)
(update :system)
(source \"https://github.com/metarepl/)")

(defvar *repocicl-dir* nil
  "all managed repositories will be cloned under this directory")

(defvar *common-lisp-dir* nil
  "pathspec to a directory where asdf looks")

(defvar *download* t)

(defvar *local-only* (uiop:getenv "REPOCICL_LOCAL_ONLY"))

(defvar *github-token* (uiop:getenv "REPOCICL_GITHUB_TOKEN"))

(defvar *verbose* nil)

(defvar *clone-systems* nil
  "a list of asdf system names to ensure are cloned")

(defvar *update-systems* nil
  "a list of asdf system names to ensure are cloned and updated")

(defvar *source-urls* nil
  "a list of github urls to search for systems below")

(defvar *declared-repocicl-systems* nil
  "a hash table of system-name:systems
which the configuration explicitly names
and that have been installed by repocicl")

(defvar *discovered-repocicl-systems* nil
  "a hash table of system-name:systems
which have been discovered by searches
and that have been installed by repocicl")

;; ----------------------------------------------------------------------
;; Low-level helpers
;; ----------------------------------------------------------------------

;; for compatibility and a potential future merge we will copy some internal functions of ocicl

(defun split-on-delimiter (line delim)
"replace ocicl"
(str:split delim line))

(defun should-log ()
"copy ocicl
Whether or not OCICL should output useful log info to *VERBOSE*."
(and *verbose* (or (eq t *verbose*) (output-stream-p *verbose*))))

(defun sanitize-system-name (name)
  "copy ocicl
Sanitize system name to prevent command injection."
  (let ((name-str (princ-to-string name)))
    ;; Only allow alphanumeric, dash, underscore, dot, plus, and slash
    (if (every (lambda (c) (or (alphanumericp c) (find c "-_.+/"))) name-str)
        name-str
(error "Invalid system name: ~A" name-str))))

(defun repo-name-from-url (url)
  (assert (str:ends-with-p ".git" url) ()
          "a git url must end with .git")
  (let* ((trimmed (string-right-trim ".git" url))
         (name (car (last (split-string trimmed :separator '(#\/))))))
    name))

(defun git-url-repocicl-repo-path (git-url)
  (ensure-directory-pathname
   (merge-pathnames (repo-name-from-url git-url) *repocicl-dir*)))

(defun symlink-path (asd-file)
   (make-pathname :defaults asd-file
                  :directory (pathname-directory *common-lisp-dir*)))

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
              (namestring (getf components :namestring))
              (namestring-resolved (getf components-resolved :namestring))
              ;; clean up, ensure ~ is converted to /home/user
              ;; without any other path resolution
              (parsedstring (pathname-utils:native-namestring
                             namestring))
              (parsedstring-resolved (pathname-utils:native-namestring
                                      namestring-resolved))

              ;; tests with :directory were the only other option
              ;; TODO still need to resolve ~ with out probes resolving the link
              ;; (pathname (make-pathname :directory (getf components :directory)
              ;;                          :name (getf components :name)
              ;;                          :type (getf components :type)))

              ;; (pathname-resolved
              ;;   (make-pathname :directory (getf components-resolved :directory)
              ;;                  :name (getf components-resolved :name)
              ;;                  :type (getf components-resolved :type)))
              )
         ;; namestring resolves to the symlink file
         ;; only when target not found
         (string= parsedstring parsedstring-resolved))))

(defun safe-delete-broken-symlink (path)
  (when (and (symbolic-link-p path)
             (symbolic-link-broken-p path))
    (delete-file path)))

(defun create-or-update-symlink (asd-file)
  (let ((link (symlink-path asd-file)))
    (safe-delete-broken-symlink link)
    ;; if it still exists, it is a good link, a real file or not ours
    (unless (probe-file link)
      (when (should-log)
        (format t "~&Repocicl: Symlinking ~A -> ~A~%" link asd-file))
       (ensure-directories-exist (directory-namestring link))
       (org.shirakumo.filesystem-utils:create-symbolic-link link asd-file))))

(defun find-top-level-asd (repo-dir system-name)
  (let ((sys (string-downcase system-name)))
    (loop for asd in (directory (merge-pathnames "*.asd" repo-dir))
          when (or (string-equal (pathname-name asd) sys)
                   (search sys (namestring asd) :test #'char-equal))
          return asd)))

(defun system-list (&key (declared t) (discovered t))
  "copy ocicl
Return list of all known system names"
  (initialize-globals)
  (append
   (when (and declared *declared-repocicl-systems*)
     (loop for key being the hash-keys of *declared-repocicl-systems*
           collect key))
   (when (and discovered *discovered-repocicl-systems*)
     (loop for key being the hash-keys of *discovered-repocicl-systems*
           collect key))))

;; ----------------------------------------------------------------------
;; core git
;; ----------------------------------------------------------------------

;;;; update system

(defun update-repo-run (program-args url directory)
  (uiop:run-program `(,@program-args ,url)
                    :directory directory
                    :output (should-log)
                    :error-output (should-log)))

(defmethod update-repo-gen (url directory (repository-type (eql :git)))
  (update-repo-run '("git" "pull") url directory))

(defmethod update-repo-gen (url directory (repository-type (eql :svn)))
  (update-repo-run '("svn" "update") url directory))

(defmethod update-repo-gen (url directory (repository-type (eql :darcs)))
  (update-repo-run '("darcs" "pull" "-a") url directory))

(defmethod update-repo-gen (url directory (repository-type (eql :hg)))
  (update-repo-run '("hg" "pull" "-u") url directory))

(defgeneric update-repo-gen (url directory repository-type)
  (:documentation "fetch and update the working tree
for an existing repository of REPOSITORY-TYPE from URL, as a subdirectory of DIRECTORY. REPOSITORY-TYPE can be :GIT, :SVN, :DARCS, or :HG"))

(defun update-repo (git-url &key (repository-type :git)
                           (target-dir *repocicl-dir*))
  "Download the ASDF-SYSTEM of REPOSITORY-TYPE from the URL"
  (let ((directory (git-url-repocicl-repo-path git-url)))
    (when (should-log)
      (format t "~&Repocicl: updating to: ~A" directory))
    (update-repo-gen url target-dir repository-type)))

;;;; clone system

(defun clone-repo-run (program-args url directory)
  (uiop:run-program `(,@program-args ,url)
                    :directory directory
                    :output (should-log)
                    :error-output (should-log)))

(defmethod clone-repo-gen (url directory (repository-type (eql :git)))
  (clone-repo-run '("git" "clone") url directory))

(defmethod clone-repo-gen (url directory (repository-type (eql :svn)))
  (clone-repo-run '("svn" "co") url directory))

(defmethod clone-repo-gen (url directory (repository-type (eql :darcs)))
  (clone-repo-run '("darcs" "get") url directory))

(defmethod clone-repo-gen (url directory (repository-type (eql :hg)))
  (clone-repo-run '("hg" "clone") url directory))

(defgeneric clone-repo-gen (url directory repository-type)
  (:documentation "clone a repository of REPOSITORY-TYPE from URL, as a
subdirectory of DIRECTORY. REPOSITORY-TYPE can be :GIT, :SVN, :DARCS, or :HG"))

(defun clone-repo (git-url &key (repository-type :git)
                           (target-dir *repocicl-dir*))
  "Download the ASDF-SYSTEM of REPOSITORY-TYPE from the URL"
  (let ((directory (git-url-repocicl-repo-path git-url)))
    (when (should-log)
      (format t "~&Repocicl: cloning to: ~A" directory))
    (clone-repo-gen git-url target-dir repository-type)
    ))


;; &&& vvv process me

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

;; &&& for clone and update because search only knows github
;; &&& if :type is not :git then url must be provided

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

;; ----------------------------------------------------------------------
;; strategy helpers
;; ----------------------------------------------------------------------

;;;; check and act
(defun system-available-p (asdf-system)
  "check if download is needed"
  (asdf:find-system asdf-system nil))

(defun make-system-available (asdf-system)
  "make system known to asdf"
  (asdf:clear-configuration)
  (asdf:find-system asdf-system))

(defun add-system (asdf-system)
  (when (not (system-available-p asdf-system))
    (progn
      (clone-system asdf-system)
      (make-system-available asdf-system))))

(defun discover-via-remote (base system-name)
  (let ((owner (car (last (split-string (string-right-trim "/" base) :separator '(#\/))))))
    (or (github-code-search owner system-name)
        ;; fallback
        (concatenate 'string base (string-downcase system-name)))))

;; ----------------------------------------------------------------------
;; Strategies
;; ----------------------------------------------------------------------

(defun strategy-ensure-cloned ()
  "Process *clone-systems*"
  (dolist (entry *clone-systems*)
    (destructuring-bind (url &key ref) (if (consp entry) entry (list entry))
      (git-clone url :ref ref))))
;; &&& make system available after

(defun strategy-ensure-updated ()
  "Process *update-systems*"
  (dolist (entry *update-systems*)
    (destructuring-bind (url &key ref) (if (consp entry) entry (list entry))
      (git-clone url :ref ref)   ; ensure present first
      (git-update url :ref ref))))
;; &&& make system available after

(defun strategy-local-discovery (system-name)
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

(defun find-asdf-system-file-old (name download-p )
  ;; &&& copy ocicl
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

(defun find-asdf-system-file (name download-p &optional (declared-p nil) (remote-p nil))
  ;; &&& copy ocicl
  "Find ASDF system file for NAME, optionally downloading if DOWNLOAD-P is true."
  ;; refresh state
  (initialize-globals)
  ;; Strategy: Ensure cloned
  (strategy-ensure-cloned)
  ;; Strategy: Ensure updated
  (strategy-ensure-updated)
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





    ;; &&& use declared-p and remote-p to filter strategies

    ;; &&& weave these strategies into the ocicl version
    ;; local and declared
    ;; (strategy-local-discovery system-name) ;; filtered for declared
    ;; ;; local
    ;; (strategy-local-discovery system-name)
    ;; ;; remote search
    ;; (strategy-remote-discovery system-name)




    (or (try-load *local-ocicl-systems* *local-systems-dir*)
        (unless *local-only*
          (try-load *global-ocicl-systems* *global-systems-dir*))
        (when download-p
          (ocicl-install name)
          (setf *local-ocicl-systems* (read-systems-csv *local-systems-csv*))
          (find-asdf-system-file name nil)))))

(defun system-definition-searcher-old (name)
  ;; &&& copy ocicl
  "Search for ASDF system definition file for NAME, using ocicl if needed."
  (unless (or (starts-with-p "asdf/" name) (string= "asdf" name) (string= "uiop" name))
    (let* ((*verbose* (or *verbose* asdf:*verbose-out*))
           (system-file (find-asdf-system-file name *download*)))
      (when (and system-file
                 (string= (pathname-name system-file) name))
        system-file))))

(defun system-definition-searcher-declared (system-name)
  " ASDF entry point
the first search method
if for systems explicitly configured: find, install and use
has the effect of shadowing any other locations"
  (format t "~&Repocicl: Declared triggered for system ~S~%" system-name)

  (unless (or (starts-with-p "asdf/" name) (string= "asdf" name) (string= "uiop" name))
    (let* ((*verbose* (or *verbose* asdf:*verbose-out*))
           ;; &&& ensure *download* is getting set by something
           (system-file (find-asdf-system-file name *download* t nil)))
      (when (and system-file
                 (string= (pathname-name system-file) name))
        system-file))))

(defun system-definition-searcher-discovered (system-name)
  "ASDF entry point
all other search methods have not returned the system
1st does repocicl have something not explicitly stated in the config
finally search *source-urls* and install if found"
  (format t "~&Repocicl: Remote search triggered for system ~S~%" system-name)

  (unless (or (starts-with-p "asdf/" name) (string= "asdf" name) (string= "uiop" name))
    (let* ((*verbose* (or *verbose* asdf:*verbose-out*))
           ;; &&& ensure *download* is getting set by something
           (system-file (find-asdf-system-file name *download* nil t)))
      (when (and system-file
                 (string= (pathname-name system-file) name))
        system-file))))

;; ----------------------------------------------------------------------
;; Config loading
;; ----------------------------------------------------------------------

(defun config-load (path)
  (when (should-log)
    (format t "~&Repocicl: Loading config ~A~%" path))
  (with-open-file (stream path :direction :input
                               :if-does-not-exist :error)
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          do (config-apply form))))

(defun config-apply (form)
  "dispatch on the expected config functions to exclude any unapproved operations"
  (case (first form)
    (repocicl:clone (apply 'repocicl:clone (rest form)))
    (repocicl:update (apply 'repocicl:update (rest form)))
    (repocicl:source (apply 'repocicl:source (rest form)))
    (t (warn "Unexpected form (not applied) in repocicl config:~%~S" form))
    ))

(defun source (url)
  (when (should-log)
    (format t "~&Repocicl: configure source: ~S" url))
  ;; &&& qc config inputs, url
  (let ((tail (string-right-trim "/" url)))
    (unless (member tail *source-urls* :test #'string=)
      (setf *source-urls*
            (append *source-urls* (list tail))))))

(defun clone (asdf-system &key type ref url)
  (when (should-log)
    (format t "~&Repocicl: configure clone: ~S" asdf-system))
  ;; &&& qc config inputs, type ref url

  (let ((tail (list :asdf-system (sanitize-system-name asdf-system)
                    :type type :ref ref :url url)))
    (unless (member tail *clone-systems* :test #'equal)
      (setf *clone-systems*
            (append *clone-systems* (list tail))))))

(defun update (asdf-system &key type ref url)
  (when (should-log)
    (format t "~&Repocicl: configure update: ~S" asdf-system))
  ;; &&& qc config inputs, type ref url
  (let ((tail (list :asdf-system (sanitize-system-name asdf-system)
                    :type type :ref ref :url url)))
    (unless (member tail *update-systems* :test #'equal)
      (setf *update-systems*
            (append *update-systems* (list tail))))))

(defun config-show ()
  "shows the result of loading the config"
  (format t "~&~%clone:~{~&~S~}" *clone-systems*)
  (format t "~&~%update:~{~&~S~}" *update-systems*)
  (format t "~&~%source:~{~&~S~}" *source-urls*))

(defun config-clear ()
  "clears lists of systems and urls that were sourced from the config,
and the lists of known systems that are derivative of the config"
  (setf *clone-systems* nil)
  (setf *update-systems* nil)
  (setf *source-urls* nil)
  (setf *declared-repocicl-systems* nil)
  (setf *discovered-repocicl-systems* nil)
  )

;; ----------------------------------------------------------------------
;; Repocicl state
;; ----------------------------------------------------------------------

(defun read-repocicl-state (&optional (repocicl-dir *repocicl-dir*))
  "introspects the *repocicl-dir* for all cloned systems
return hash table mapping system-name -> (version . path)
where version is the git hash
"
  (when (should-log)
    (format *verbose* "Repocicl: assessing ~A~%" repocicl-dir))
  (let ((ht (make-hash-table :test #'equal))
        (cloned-directories (uiop:subdirectories repocicl-dir)))
    (dolist (dir cloned-directories)
      ;; TODO prevent the ref generation when not a git repo or skip on the error
      (let ((ref (str:trim (run-program '("git" "rev-parse" "HEAD")
                                        :output :string
                                        :directory dir)))
             (asd-files  (directory-files dir "*.asd")))
        (dolist (asd-file asd-files)
          (let ((system-name (pathname-name asd-file)))
            (setf (gethash system-name ht) (cons ref asd-file))))))
    ht))

;; ----------------------------------------------------------------------
;; Setup
;; ----------------------------------------------------------------------

;; TODO test that ocicl is available
;; we assume ocicl has been setup and loaded successfuly before reposicle

(defun ensure-dirs ()
  (ensure-directories-exist *config-path*)
  (ensure-directories-exist *repocicl-dir*)
  (ensure-directories-exist *common-lisp-dir*))

(defun ensure-config ()
  ;; the config exists or a default is placed
  (unless (probe-file *config-path*)
    (when (should-log)
      (format t "Repocicl: creating a default config at XDG_CONFIG_HOME/repocicl"))
    (uiop:copy-file
     (asdf:system-relative-pathname :metarepl.systems.repocicl
                                    "src/config-template.lisp")
     *config-path*))
  ;; the config is loaded whenever any one is null
  (when (or (null *clone-systems*)
            (null *update-systems*)
            (null *source-urls*))
    (config-load *config-path*))
  *config-path*)

(defun initialize-globals ()
  "copy ocicl
Initialize global variables, and introspect the systems repocicl is managing relative to the configuration"

  (unless *config-path*
    (setf *config-path*
          (merge-pathnames "repocicl/config.lisp" (uiop:xdg-config-home))))

  (unless *repocicl-dir*
    (setf *repocicl-dir*
          (merge-pathnames "repocicl/" (user-homedir-pathname))))

  (unless *common-lisp-dir*
    (setf *common-lisp-dir*
          (merge-pathnames "common-lisp/" (user-homedir-pathname))))

  (ensure-dirs)
  (ensure-config)

  ;; TODO make separate responses in for discovered and declared
  ;; copy ocicl (setf *local-ocicl-systems* (read-systems-csv *local-systems-csv*))
  ;; *discovered-repocicl-systems* in config, high priority return, shadows other searches
  ;; *declared-repocicl-systems* not in config, low priority return, shadowed by other searches
  (setf *declared-repocicl-systems* (read-repocicl-state))
  (setf *discovered-repocicl-systems* (read-repocicl-state)))

(defun setup ()
  (initialize-globals)

  (defvar *original-system-definition-search-functions* asdf:*system-definition-search-functions*
    "preserve original asdf:*system-definition-search-functions* before repocicl makes modifications")


  ;; ;; register local search
  ;; (unless (member 'system-definition-searcher-declared
  ;;                 asdf:*system-definition-search-functions*)
  ;;   ;; highest priority at head of list
  ;;   (pushnew 'system-definition-searcher-local asdf:*system-definition-search-functions*))

  ;;   ;; register online search
  ;; (unless (member 'system-definition-searcher-discovered
  ;;                 asdf:*system-definition-search-functions*)
  ;;   ;; lowest priority at the tail of search list
  ;;   (setf asdf:*system-definition-search-functions*
  ;;         (append asdf:*system-definition-search-functions*
  ;;                 (list 'system-definition-searcher-remote))))

  ;; &&& for now just use a simple search
  (defvar *original-central-registry* asdf:*central-registry*
    "preserve original asdf:*central-registry* before repocicl makes modifications")
  (pushnew *repocicl-dir* asdf:*central-registry*)

  (pushnew :REPOCICL *features*))

(eval-when (:execute)
  (setup))
