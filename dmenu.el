(require 'ido)
(defgroup dmenu nil
  "M-x interface with Ido-style fuzzy matching and ranking heuristics."
  :group 'extensions
  :group 'convenience
  :link '(emacs-library-link :tag "Lisp File" "dmenu.el"))

(defcustom dmenu-save-file (locate-user-emacs-file "dmenu-items")
  "File in which the dmenu state is saved between Emacs sessions.
Variables stored are: `dmenu-data', `dmenu-history'.
Must be set before initializing Dmenu."
  :type 'string
  :group 'dmenu)

(defun dmenu(&optional prefix)
  (interactive "p")
  (unless dmenu-initialized-p
	(dmenu-initialize))
  (unless dmenu--cache-executable-files
	(dmenu--cache-executable-files))
  (let* ((execute-file (ido-completing-read+ ": " dmenu--cache-executable-files nil 'confirm nil 'dmenu-history-list))
		 (args))
	(when (= prefix 4)
	  (setq args (read-string "请输入参数: "))
	  (with-temp-buffer
	  	(insert args)
	  	(setq args (car (shell--parse-pcomplete-arguments)))))
	(setq dmenu-history-list (remove execute-file dmenu-history-list))
	(push execute-file dmenu-history-list)
	(switch-to-buffer (apply #'make-comint execute-file execute-file nil args))))

(defvar dmenu-initialized-p nil)

;;;###autoload
(defun dmenu-initialize ()
  (interactive)
  (unless ido-mode (dmenu-initialize-ido))
  (dmenu-load-save-file)
  (dmenu-auto-update)
  (add-hook 'kill-emacs-hook 'dmenu-save-to-file)
  (setq dmenu-initialized-p t))

(defun dmenu-initialize-ido ()
  "Sets up a minimal Ido environment for `ido-completing-read'."
  (ido-init-completion-maps)
  (add-hook 'minibuffer-setup-hook 'ido-minibuffer-setup))


(defun dmenu-load-save-file ()
  "Loads `dmenu-history' and `dmenu-data' from `dmenu-save-file'"
  (let ((save-file (expand-file-name dmenu-save-file)))
    (if (file-readable-p save-file)
        (with-temp-buffer
          (insert-file-contents save-file)
		  (ignore-errors
			(setq dmenu--cache-executable-files (read (current-buffer)))))
      (setq dmenu-history nil dmenu-data nil))))

(defun dmenu-save-to-file ()
  (interactive)
  (dmenu-save-history)
  (with-temp-file (expand-file-name dmenu-save-file)
    (ido-pp 'dmenu--cache-executable-files)))


(defvar dmenu-history-list nil)

(defvar dmenu--cache-executable-files nil)

(defun dmenu--cache-executable-files()
  "缓存可执行文件列表"
  (let* ((valid-exec-path (remove-if-not #'file-exists-p (remove-if-not #'stringp exec-path)))
		 (files (mapcan (lambda (dir)
						  (directory-files dir t nil nil)) valid-exec-path)))
		 (setq dmenu--cache-executable-files (sort (mapcar #'file-name-nondirectory (remove-if #'file-directory-p (remove-if-not #'file-executable-p files))) #'string< ))))

(defvar dmenu--update-timer nil)

(defun dmenu-auto-update (&optional idle-time)
  "Update dmenu when Emacs has been idle for IDLE-TIME."
  (unless idle-time (setq idle-time 60))
  (when dmenu--update-timer
	(cancel-timer dmenu--update-timer))
  (setq dmenu--update-timer (run-with-idle-timer idle-time t
                       #'dmenu--cache-executable-files)))

(provide 'dmenu)
