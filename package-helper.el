(defun package-installable-p (package)
  "检查package是否有安装源"
  (require 'package)
  (unless package--initialized
	(package-initialize t))
  (unless package-archive-contents
	(package-refresh-contents))
	(memq package (mapcar #'car package-archive-contents)))

(defun package-loadable-p (package)
  "判断`package'是否能被加载"
  (require 'cl)
  (let ((load-file (concat (format "%s" package) ".el")))
	(cl-some (lambda (dir)
			   (file-exists-p (expand-file-name load-file dir))) load-path)))

(defun package-install-new (package &optional min-version no-refresh)
  "当不存在package时才安装package

Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun require-and-install (pkg &optional filename noerror)
  ""
  (unless (require pkg filename t)
	(when (and (package-installable-p pkg)
			   (not (package-installed-p pkg)))
	  (package-refresh-contents)
	  (package-install  pkg)
	  (require pkg filename noerror))))

(require-and-install 'async)
(defun package-list-packages-async ()
  "package-install async"
  (interactive)
  (async-start
   `(lambda ()
      (setq package-archives
            ',package-archives)
      (require 'finder-inf nil t)
      ;; Initialize the package system if necessary.
      (package-initialize t)
      (let (old-archives new-packages)
		;; Read the locally-cached archive-contents.
		(package-read-all-archive-contents)
		(setq old-archives package-archive-contents)
		;; Fetch the remote list of packages.
		(package-refresh-contents)
		;; Find which packages are new.
		(dolist (elt package-archive-contents)
		  (unless (assq (car elt) old-archives)
			(push (car elt) new-packages)))
        (setq result-prev (list new-packages package-archive-contents)))
      )
   `(lambda (result)
      (setq package-archive-contents (cadr result))
      (let ((new-packages (car result)))
        ;; Generate the Package Menu.
        (let ((buf (get-buffer-create "*Packages*")))
          (with-current-buffer buf
            (package-menu-mode)
            (set (make-local-variable 'package-menu--new-package-list)
                 new-packages)
            (package-menu--generate nil t))
          ;; The package menu buffer has keybindings.  If the user types
          ;; `M-x list-packages', that suggests it should become current.
          (switch-to-buffer buf))

        (let ((upgrades (package-menu--find-upgrades)))
          (if upgrades
              (message "%d package%s can be upgraded; type `%s' to mark %s for upgrading."
                       (length upgrades)
                       (if (= (length upgrades) 1) "" "s")
                       (substitute-command-keys "\\[package-menu-mark-upgrades]")
                       (if (= (length upgrades) 1) "it" "them"))))))))

(defun package-install-async (package)
  "异步安装package"
  (interactive "Swhich package do you want to install?")
  (async-start
   `(lambda ()
      (setq package-archives
            ',package-archives)
      ;; Initialize the package system if necessary.
      (package-initialize t)
	  (package-install ',package))
   `(lambda (result)
	 (package-initialize nil)
	 (message "%s installed" ',package))))


(defun sanityinc/set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (cl-loop for column across tabulated-list-format
           when (string= col-name (car column))
           do (setf (elt column 1) width)))

(defun sanityinc/maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (sanityinc/set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
      (sanityinc/set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook 'sanityinc/maybe-widen-package-menu-columns)

(provide 'package-helper)
