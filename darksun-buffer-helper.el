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
