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
(defcustom local-remote-root-pairs '(("e:/git-svn/server/" . "/cnaps2@10.8.6.10:")
									 ("d:/workcvs/ibps/ibps" . "/ibpsusr@10.8.6.10:"))
  "本地项目root与对应的remote地址的alist

其中remote-root的格式为/username@ip:/path/to/root/
以local-root开头的文件会被ftp到对应的remote-root上去")


(defun local-path-to-remote-path(local-path)
  "转换本地路径为ftp的远程路径"
  (some (lambda (local-remote-root-pair)
		  (let ((local-root (car local-remote-root-pair))
				(remote-root (cdr local-remote-root-pair)))
			(if (string-prefix-p local-root local-path)
				(replace-regexp-in-string (concat "^" (regexp-quote local-root)) remote-root local-path)
			  nil)))
		local-remote-root-pairs)) 

;; (local-path-to-remote-path "e:/git-svn/server/trunk/makeall")
(global-set-key [f11] 'UpMeToFtp)
(defun UpMeToFtp()
  "Upload me to the ftp "
  (interactive)
  (let (remote-path)
	(setq remote-path (local-path-to-remote-path buffer-file-name))
  (if remote-path
	  (copy-file buffer-file-name  remote-path t))))  

(provide 'autoftp)
