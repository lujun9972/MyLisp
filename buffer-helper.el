;; Buffer相关函数
(defun buffer-contents (&optional buffer-name)
  "获取指定buffer的内容"
  (let (buffer)
	(when (null buffer-name)
	  (setq buffer-name (buffer-name)))
	(setq buffer (get-buffer(format "%s" buffer-name)))
	(when buffer
	  (with-temp-buffer
		(insert-buffer buffer)
		(buffer-string)))))

(defun buffer-contents-to-list (buffer-name)
  "读取buffer的内容到list中"
  (split-string (buffer-contents buffer-name) "[\n\r]+"))

(defmacro do-buffer-line (var buffer-name  &rest body)
  "针对buffer的每一行内容作操作,类似dolist"
  `(dolist (,var (buffer-line-to-list ,buffer-name)) 
	 ,@body))

(defun bounds-of-region-or-thing (THING)
  "若有active region则返回region的起始位置,否则返回光标所代表THING的位置"
  (if (region-active-p) 
	  (cons (region-beginning) (region-end))
	(bounds-of-thing-at-point THING)))

(defun get-region-or-thing (THING)
  "若有active region则返回region的起始位置,否则返回光标所代表THING的内容"
  (if (region-active-p) 
	  (buffer-substring-no-properties (region-beginning) (region-end))
	(thing-at-point THING)))

(defun buffer-point(buffer-or-name &optional default-point)
  "获取指定buffer的光标所在位置

若参数`buffer-or-name'没有对应buffer,则返回`default-point'"
  (if (get-buffer buffer-or-name)
	  (with-current-buffer buffer-or-name
		(point))
	default-point))

(defun select-or-create-buffer-window (buffer-or-name)
  "若frame中有显示`buffer-or-name'的window,则选中该window,否则创建新window显示该buffer"
  (let ((buf (get-buffer-create buffer-or-name)))
	(unless (get-buffer-window buf)
	  (split-window)
	  (switch-to-buffer buf))
	(select-window (get-buffer-window buf))))


(defun fontify-block (start end &optional lang)
  "Fontify code block."
  (interactive "r")
  (let* ((lang (or lang (read-string "which mode? ")))
		 (lang-mode (intern (format "%s-mode" (replace-regexp-in-string "-mode$" "" lang)))))
    (if (fboundp lang-mode)
		(let ((string (buffer-substring-no-properties start end))
			  (modified (buffer-modified-p))
			  (origin-buffer (current-buffer)) pos next)
		  (remove-text-properties start end '(face nil))
		  (with-temp-buffer
			(insert string " ") ;; so there's a final property change
			(unless (eq major-mode lang-mode) (funcall lang-mode))
			(font-lock-fontify-buffer)
			(setq pos (point-min))
			(while (setq next (next-single-property-change pos 'face))
			  (put-text-property
			   (+ start (1- pos)) (1- (+ start next)) 'face
			   (get-text-property pos 'face) origin-buffer)
			  (setq pos next)))
		  ;; (add-text-properties
		  ;;  start end
		  ;;  '(font-lock-fontified t fontified t font-lock-multiline t))
		  (set-buffer-modified-p modified)))))

(provide 'buffer-helper)
