(require 'cl)
(require 'eshell)
(defvar eshell-auto-executable-commands '(ei )
  "允许自动执行的command")

(defun eshell-auto-execute-commands ()
  "自动执行command"
  (when (and (eq major-mode 'eshell-mode)
			 (cl-member-if (lambda (x)
							 "判断已输入的命令是否在`eshell-auto-executable-commands'中"
							 (string= (eshell-get-old-input) (format "%s" x)))
						   eshell-auto-executable-commands))
	(eshell-send-input)))

(defun turn-on-eshell-auto-execute-commands ()
  "开启自动执行command

开启后,只要输入的命令在变量`eshell-auto-executable-commands'中,则会自动执行,不需要按回车"
  (interactive)
  (unless (eq major-mode 'eshell-mode)
	(error "只能在eshell-mode下开启自动执行command"))
  (add-hook 'post-self-insert-hook #'eshell-auto-execute-commands t t))

(defun turn-off-eshell-auto-execute-commands ()
  "关闭自动执行command"
  (interactive)
  (remove-hook 'post-self-insert-hook #'eshell-auto-execute-commands t))

(provide 'eshell-auto-execute-commands)
