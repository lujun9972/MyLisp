(defun url-get-content-from-html-async (url)
  ""
	(url-retrieve url (lambda (status)
						(setq url-content (libxml-parse-xml-region (point-min) (point-max))))))

(defun url-get-content-from-html(url)
  "从http `url'中获取经过`libxml-parse-html-region'解析的内容"
  (let ((url-buffer (url-retrieve-synchronously url))
		url-content)
	(with-current-buffer url-buffer
	  (setq url-content (libxml-parse-html-region (point-min) (point-max))))
	(kill-buffer url-buffer)
	url-content))
(defun url-get-thing-from-html(url tag &optional attr regexp)
  "从http `url'中tag标签下,attr属性且获取匹配`regexp'的值. 其中attr表示属性时,格式为":属性"

`url'可以是一个代表url的字符串,或者url-content. `attr'默认为'text,表示tag标签保住的内容,`regexp'默认为空字符串,表示匹配值"
  (unless attr
	(setq attr 'text))
  (unless regexp
	(setq regexp ""))
  (let ((url-content (if (stringp url)
						 (url-get-content-from-html url)
					   url))
		url-things)
	(with-temp-buffer 
	  (flet ((tag-node-p (node)
						 (listp (cdr node)))
			 (attr-node-p (node)
						 (atom (cdr node)))
			 (save-url-thing-fn (cont &optional ignore)
								"`ignore'参数指定了是否跳过tag node的attr判断. 当处理子tag时,需要根据子tag是否为需要的tag来设置该值."
								(dolist (sub cont)
								  (cond ((and (attr-node-p sub)
											  (not ignore))
										 (when (and (eq (car sub) attr)
													(string-match-p regexp (cdr sub)))
										   (push (cdr sub) url-things)))
										((tag-node-p sub)
										 (let ((things (save-url-thing-fn (cdr sub) (not (eq (car sub) tag)))))
										   (when things
											 (push things url-things)))))))) ;处理嵌套tag
		(let ((shr-external-rendering-functions `((,tag . save-url-thing-fn))))
		  (shr-insert-document url-content))))
	url-things))

(defun url-get-title-from-html(url)
  "从http url中获取title"
  (url-get-thing-from-html url 'title))

(defun url-get-links-from-html(url &optional regexp)
  "从http `url'中获取匹配`regexp'的超链接地址"
  (url-get-thing-from-html url 'a :href regexp))

(defun url-get-images-from-html(url &optional regexp)
  "从http `url'中获取匹配`regexp'的图片地址"
  (url-get-thing-from-html url 'img :src regexp))

(provide 'url-helper)
