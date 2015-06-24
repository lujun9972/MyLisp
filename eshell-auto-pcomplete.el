(require 'eshell)

(defun ido-pcomplete ()
  "使用ido作为eshell的pcomplete方法"
  (interactive)
;; @ To simplify completion function logic, the tag `pcompleted' may
;;   be thrown with a value of nil in order to abort the function.  It
;;   means that there were no completions available.
  (catch 'pcompleted
	(let* (completion-result
		   (completions (pcomplete-completions))
		   (candidates (all-completions pcomplete-stub completions))
		   (pcomplete-stub (replace-regexp-in-string ".*[\/]" "" pcomplete-stub))
		   )
	  (cond ((null candidates)
			 (error "没有匹配项"))
			((= 1 (length candidates))
			 (setq completion-result (car candidates)))
			(t (setq completion-result (ido-completing-read ": " candidates nil nil pcomplete-stub))))
	  (delete-char (- (length pcomplete-stub)))
	  (insert completion-result))))

(when eshell-mode-map
  (define-key eshell-mode-map (kbd "<tab>") 'ido-pcomplete))

(defvar pcomplete-fn 'eshell-pcomplete
  "用于补全的方法
目前可以选择'eshell-pcomplete或'ido-pcomplete.推荐'eshell-pcomplete")

(defun turn-on-eshell-auto-pcomplete ()
  "开启自动执行pcomplete补全

开启后,每输入一个字符,都会自动调用eshell-pcomplete进行补全"
  (interactive)
  (unless (eq major-mode 'eshell-mode)
	(error "只能在eshell-mode下开启自动执行command"))
  (add-hook 'post-self-insert-hook pcomplete-fn t t))

(defun turn-off-eshell-auto-pcomplete ()
  "关闭自动执行pcomplete不全"
  (interactive)
  (remove-hook 'post-self-insert-hook pcomplete-fn t))

(provide 'eshell-auto-pcomplete)
