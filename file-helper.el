(defun file-size(file-path)
  "获取文件的字节数"
  (nth 7 (file-attributes file-path)))

(defun file-concat(dir file)
  "连接文件路径"
  (concat (file-name-as-directory dir) file))

(defun file-md5(path)
  "计算文件内容的md5"
  (with-temp-buffer
	(insert-file-contents-literally path)
	(md5 (buffer-string))))

(defun file-content(file)
  "返回file的文件内容"
  (with-temp-buffer
	(insert-file-contents file)
	(buffer-string)))

(defun file-name-directory-name-of-path (path)
  "文件所属的目录名称,仅仅只是包含该文件的目录名称"
  (let ((dir (file-name-directory path))) 
	(if (string-match ".+[/\\]\\([^/\\]+\\)[/\\]$" dir)
		(match-string 1 dir)
	  dir)))

(defun files-in-directory-with-subdir (dir &optional pred full match nosort)
  "类似directory-files,但是会递归搜索子目录,且返回的是符合`pred'判断的文件,`pred'默认为除了.和..之外的所有文件"
  (unless pred
	(setq pred (lambda (file)
				 (not (string-match-p "/\\..*$" file)))))
  (when (file-directory-p dir)
	(let* ((files (remove-if-not pred (directory-files dir t match t)))
		   (dirs (remove-if (lambda (dir)
						   (string-match-p "/\\..*$" dir)) (remove-if-not #'file-directory-p (directory-files dir t)))))
	  (setq files (append files (mapcan (lambda (dir)
										  (files-in-directory-with-subdir dir pred t match t)) dirs)))
	  (unless full
	  	(setq files (mapcar (lambda (file)
							  (file-relative-name file dir))
							files)))
	  (unless nosort
		(setq files (sort files #'string<)))
	  files)))

(defun open-current-file-in-clean-emacs ()
  ""
  (interactive)
  (let ((emacs-bin-path (concat invocation-directory invocation-name))
        (file (buffer-file-name)))
    (if file
        (start-process file nil emacs-bin-path "-Q" file)
      (error "当前buffer没有对应的file"))))

(defun emacs ()
  ""
  (interactive)
  (let ((emacs-bin-path (concat invocation-directory invocation-name)))
    (start-process "emacs" nil emacs-bin-path )))

;; 以下函数摘自李杀网
(defun fullpath-relative-to-current-file (file-relative-path)
  "Returns the full path of FILE-RELATIVE-PATH, relative to file location where this function is called.

Example: If you have this line
 (fullpath-relative-to-current-file \"../xyz.el\")
in the file at
 /home/mary/emacs/emacs_lib.el
then the return value is
 /home/mary/xyz.el
Regardless how or where emacs_lib.el is called.

This function solves 2 problems.

 ① If you have file A, that calls the `load' on a file at B, and
B calls “load” on file C using a relative path, then Emacs will
complain about unable to find C. Because, emacs does not switch
current directory with “load”.

 To solve this problem, when your code only knows the relative
path of another file C, you can use the variable `load-file-name'
to get the current file's full path, then use that with the
relative path to get a full path of the file you are interested.

 ② To know the current file's full path, emacs has 2 ways:
`load-file-name' and `buffer-file-name'. If the file is loaded
by “load”, then load-file-name works but buffer-file-name
doesn't. If the file is called by `eval-buffer', then
load-file-name is nil. You want to be able to get the current
file's full path regardless the file is run by “load” or
interactively by “eval-buffer”."
  (file-concat (or load-file-name buffer-file-name) file-relative-path))


(defun ergoemacs-open-in-external-app ()
  "使用外部程序打开dired-mode中被标记的文件"
  (interactive)
  (let ( doIt
		 (myFileList
		  (cond
		   ((string-equal major-mode "dired-mode") (dired-get-marked-files))
		   (t (list (buffer-file-name))) ) ) )
    
	(setq doIt (if (<= (length myFileList) 5)
				   t
				 (y-or-n-p "Open more than 5 files?") ) )
    
	(when doIt
	  (cond
	   ((string-equal system-type "windows-nt")
		(mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList)
		)
	   ((string-equal system-type "darwin")
		(mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
	   ((string-equal system-type "gnu/linux")
		(mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList) ) ) ) ) )


(defun ergoemacs-open-in-desktop ()
  "使用系统的文件管理器打开文件"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
	(w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
	(let ((process-connection-type nil)) (start-process "" nil "xdg-open" "."))
	;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ⁖ with nautilus
	) ))



(provide 'file-helper)
