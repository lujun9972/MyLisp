;;自动上传文件
;设置ftp程序的地址
;(setq ange-ftp-ftp-programe-name "d:/bin/ftp_xp.exe")
(defun turn-on-auto-ftp()
  "设置打开自动ftp功能"
  (interactive)
  (add-hook 'after-save-hook 'UpMeToFtp))
(defun turn-off-auto-ftp()
  "设置管理自动ftp功能"
  (interactive)
  (remove-hook 'after-save-hook 'UpMeToFtp))

(defgroup autoftp-group nil "自动ftp到remote machine")
(defcustom autoftp-local-remote-root-alist nil 
  "本地项目root与对应的remote地址的alist

其中remote-root的格式为username ip /path/to/root/
以local-root开头的文件会被ftp到对应的remote-root上去")

(defun autoftp--make-remote-path(username ip path-to-root &optional change-ip)
  "拼出远程tramp路径"
  (when change-ip
	(setq ip (read-string "请输入远程ip:")))
  (format "/%s@%s:%s" username ip path-to-root))

(defun autoftp--local-path-to-remote-path(local-path &optional change-ip)
  "转换本地路径为ftp的远程路径"
  (some (lambda (local-remote-root-pair)
		  (let* ((local-root (car local-remote-root-pair))
				 (remote-root-elements (cdr local-remote-root-pair))
				 (username (first remote-root-elements))
				 (ip (second remote-root-elements))
				 (path-to-root (third remote-root-elements))
				 (remote-root (autoftp--make-remote-path username ip path-to-root change-ip)))
			(if (string-prefix-p local-root local-path)
				(replace-regexp-in-string (concat "^" (regexp-quote local-root)) remote-root local-path)
			  nil)))
		autoftp-local-remote-root-alist)) 

(global-set-key [f11] 'UpMeToFtp)
(defun UpMeToFtp(&optional change-ip)
  "Upload me to the ftp "
  (interactive "p")
  (let (remote-path)
	(setq remote-path (autoftp--local-path-to-remote-path buffer-file-name change-ip))
	(if remote-path
		(copy-file buffer-file-name  remote-path t))))  

(provide 'autoftp)
