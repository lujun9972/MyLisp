(require 'eshell)

(defvar pcomplete-fn 'eshell-pcomplete
  "用于补全的方法")

(defun turn-on-eshell-auto-pcomplete ()
  "开启自动执行pcomplete补全

开启后,每输入一个字符,都会自动调用`pcomplete-fn'中定义的补全函数进行补全"
  (interactive)
  (unless (eq major-mode 'eshell-mode)
	(error "只能在eshell-mode下开启自动执行command"))
  (add-hook 'post-self-insert-hook pcomplete-fn t t))

(defun turn-off-eshell-auto-pcomplete ()
  "关闭自动执行pcomplete不全"
  (interactive)
  (remove-hook 'post-self-insert-hook pcomplete-fn t))

(provide 'eshell-auto-pcomplete)
