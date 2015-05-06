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
            '(("gnu"         . "http://elpa.gnu.org/packages/")
              ("org"         . "http://orgmode.org/elpa/")
              ("melpa"       . "http://melpa.milkbox.net/packages/")))
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
(defun package-install-new (package)
  "当不存在package时才安装package"
  (when (and  (not (package-installed-p package))
			  (package-installable-p package))
	(package-install package)))

(provide 'package-helper)
