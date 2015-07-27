(require 'ido)
(defvar dmenu-history-list nil)
(defvar dmenu-output-buffer "*dmenu output*")
(defvar dmenu--cache-executable-files nil)
(defun dmenu--cache-executable-files()
  "缓存可执行文件列表"
  (let* ((files (mapcan (lambda (dir)
						  (directory-files dir t nil nil)) (remove-if-not #'file-exists-p (remove-if-not #'stringp exec-path)))))
		 (setq dmenu--cache-executable-files (sort (mapcar #'file-name-nondirectory (remove-if #'file-directory-p (remove-if-not #'file-executable-p files))) #'string< ))))

(defvar dmenu--update-timer nil)
(defun dmenu-auto-update (&optional idle-time)
  "Update dmenu when Emacs has been idle for IDLE-TIME."
  (unless idle-time (setq idle-time 60))
  (when dmenu--update-timer
	(cancel-timer dmenu--update-timer))
  (setq dmenu--update-timer (run-with-idle-timer idle-time t
                       #'dmenu--cache-executable-files)))

(defun dmenu(&optional prefix)
  (interactive "p")
  (unless dmenu--cache-executable-files
	(dmenu-auto-update)
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
	(apply #'start-process execute-file dmenu-output-buffer execute-file args)))


(provide 'dmenu)
