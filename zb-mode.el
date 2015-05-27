(defvar-local zb-content ""
  "zb时输入的文本")

(defun zb-self-insert-command-advise(&optional N)
  ""
  (interactive "P")
  (if (string= zb-content "")
	  (progn 
		(turn-off-zb-mode)
		(self-insert-command (or N 1)))
	(insert (elt zb-content 0))
	(setq zb-content (substring zb-content 1))))

(defun turn-on-zb-mode()
  "开启zb模式"
  (interactive)
  (when (string= zb-content "")
	(if (= (point) (point-max))
		(progn
		  (require 'file-helper)
		  (setq zb-content (file-content (read-file-name "从哪个文件导入装逼的内容?"))))
	  (setq zb-content (buffer-substring-no-properties (point) (point-max)))
	  (delete-region (point) (point-max))))
  (advice-add 'self-insert-command :override #'zb-self-insert-command-advise))

(defun turn-off-zb-mode()
  "关闭zb模式"
  (interactive)
  (advice-remove 'self-insert-command #'zb-self-insert-command-advise))

(require 'zb-mode)
