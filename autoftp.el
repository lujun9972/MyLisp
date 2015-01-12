;;自动上传文件
;设置ftp程序的地址
;(setq ange-ftp-ftp-programe-name "d:/bin/ftp_xp.exe")
(defvar *is-auto-ftp* nil
  "是否自动ftp的标志")
(defun turn-on-auto-ftp()
  "设置打开自动ftp功能"
  (interactive)
  (setf *is-auto-ftp* t))
(defun turn-off-auto-ftp()
  "设置管理自动ftp功能"
  (interactive)
  (setf *is-auto-ftp* nil))

(defvar *remote-root* "/cnaps2@10.8.6.10:"
  "远程ftp的根目录地址,格式为/username@ip:/path/to/root/")
(defun set-remote-root(remote-root)
  "设置远程ftp的根目录地址"
  (interactive "s请输入远程ftp的地址,格式为/username@ip:/path/to/root/")
  (setf *remote-root* (expand-file-name (file-name-as-directory remote-root))))

(defvar *local-root* (file-name-as-directory (expand-file-name "."))
  "*标识本地文件的开始根目录,以这些路径开头的文件才会ftp到远程")	
(defun set-local-root(local-root)
  "设置本地文件的开始根目录"
  (interactive "s请输入本地文件的开始根目录")
  (setf *local-root* (file-name-as-directory local-root)))

(defun local-path-to-remote-path(local-path)
  "转换本地路径为ftp的远程路径"
  (replace-regexp-in-string (concat "^" (regexp-quote *local-root*)) *remote-root* local-path)) 

;; (local-path-to-remote-path "d:/workcvs/cnaps2/server/trunk/makeall")
(global-set-key [f11] 'UpMeToFtp)
(defun UpMeToFtp()
  "Upload me to the ftp "
  (interactive)
  (if (and *is-auto-ftp* (string-prefix-p *local-root* buffer-file-name))
	  (copy-file buffer-file-name (local-path-to-remote-path buffer-file-name) t)))  

(add-hook 'after-save-hook 'UpMeToFtp)

(provide 'autoftp)
