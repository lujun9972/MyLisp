(require 'file-helper)
(defun proc-info--typeof (obj)
  (cond ((and (stringp obj)
			  (file-directory-p obj))
		 'directory)
		((and (stringp obj)
			  (file-exists-p obj))
		 'file)
		(t 'text)))

(defun proc-info--file-content-match(file regex)
  (let* ((file-lines (split-string (file-content file) "[\r\n]+")))
	(remove-if-not (lambda (line)
					 (string-match-p regex line))
				   file-lines)))

(defun proc-info (&rest args)
  "获取/proc下的信息"
  (let ((result "/proc"))
	(dolist (arg args)
	  (cond ((eq 'directory (proc-info--typeof result))
			 (setq result (concat (file-name-as-directory result) arg)))
			((eq 'file (proc-info--typeof result))
			 (setq result (proc-info--file-content-match result arg)))
			((listp result)
			 (setq result (remove-if-not (lambda (line)
										   (string-match-p arg line))
										 result)))
			(t (error "wrong arg type:%s,result:%s" arg result)))
	  )
	(proc-info--show result)))

(defun proc-info--show (obj)
  (cond ((eq 'directory (proc-info--typeof obj))
		 (string-join (directory-files obj nil "[^.].+") "\n")) ;不显示以.开头的文件
		((eq 'file (proc-info--typeof obj))
		 (file-content obj))
		(t (string-join obj "\n"))))


(provide 'proc-info)
