
(defparameter *repocicl-dir*
  (merge-pathnames "repocicl/" (user-homedir-pathname)))

(defun add-system (asdf-system url &optional (repository-type :git) (repocicl-dir *repocicl-dir*))
  "Download the ASDF-SYSTEM of REPOSITORY-TYPE from the URL
 unless it's already defined."
  (if (asdf:find-system asdf-system nil)
      ;; no download needed
      (asdf:load-system asdf-system)
      ;; download needed
      (progn
        (let ((asdf-system-path (merge-pathnames
                                 (make-pathname :directory `(:relative ,asdf-system)
                                                :name asdf-system
                                                :type "asd")
                                 repocicl-dir)))
          ;; download
          (download-repo repository-type url *repo-dir*)
          ;; and load
          (asdf:find-system asdf-system)
          (asdf:load-system asdf-system)
          ))))

(defgeneric download-repo (repository-type url directory)
  (:documentation "Download a repository of REPOSITORY-TYPE from URL, as a
subdirectory of DIRECTORY. REPOSITORY-TYPE can be :GIT, :SVN, :DARCS, or :HG"))

(defun download-repo-helper (program-args directory url)
  (uiop:run-program `(,@program-args ,url)
                    :directory directory
                    :output :interactive
                    :error-output :interactive))

(defmethod download-repo ((repository-type (eql :git)) url directory)
  (download-repo-helper '("git" "clone") directory url))

(defmethod download-repo ((repository-type (eql :svn)) url directory)
  (download-repo-helper '("svn" "co") directory url))

(defmethod download-repo ((repository-type (eql :darcs)) url directory)
  (download-repo-helper '("darcs" "get") directory url))

(defmethod download-repo ((repository-type (eql :hg)) url directory)
  (download-repo-helper '("hg" "clone") directory url))
