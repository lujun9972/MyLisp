;; 设置为org文件添加attachment的方法
(require 'org-attach)
(defun org-insert-attachment-link (&optional attachment-file-name)
  " Org file中便捷创建指向attachment目录中文件的链接"
  (interactive)
  (let (attachment-directory attachment-file-path)
	(setq attachment-directory (org-attach-expand ""))
	(if (null attachment-file-name)
		(setq attachment-file-path (read-file-name "请输入附件名称: " attachment-directory))
	  (setq attachment-file-path (concat (file-name-as-directory attachment-directory)  attachment-file-name))
	  )
	(org-insert-link nil (concat "./" (file-relative-name attachment-file-path)) nil)
	(newline)))

(defun org-add-attachment-and-link (&optional attachment-file-path)
  " Org file中添加附件,并增加指向附件的链接"
  (interactive)
  (let (attachment-file-name)
	(unless attachment-file-path
	  (setq attachment-file-path (read-file-name "请输入要添加为附件的文件路径")))
	(setq attachment-file-name (file-name-nondirectory attachment-file-path))
	(org-attach-attach attachment-file-path nil 'cp)
	(org-insert-attachment-link attachment-file-name)))

(defun org-add-attachment-and-link-drag-n-drop-event (event)
  "用darg-and-drop的方式,为Org file添加附件,并增加指向附件的链接"
  (interactive "e")
  (let ((files (nth 2 event)))
		(message "add %s..." files)
		(cond ((listp files)
			   (mapcar 'org-add-attachment-and-link files))
			  (t (error "未知的文件列表格式")))))


(defun org-add-attachment-and-link-by-drag-n-drop ()
  "用darg-and-drop的方式,为Org file添加附件,并增加指向附件的链接"
  (interactive)
  (let (old-drag-n-drop-event-handler)
	(setq old-drag-n-drop-event-handler (lookup-key special-event-map [drag-n-drop]))
	(unwind-protect (progn
					  (define-key special-event-map [drag-n-drop] 'org-add-attachment-and-link-drag-n-drop-event)
					  (message "可以开始拖拽要添加非附件的文件了,完成后按回车")
					  (read-char))
	  (define-key special-event-map [drag-n-drop] old-drag-n-drop-event-handler))))

;; (define-key special-event-map [drag-n-drop] 'x-dnd-handle-drag-n-drop-event)

(provide  'darksun-org-helper)
