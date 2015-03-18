(defun goto-tag-of-current-word()
  "查找当前光标下的tag"
  (interactive)
  (let ((word (current-word)))
	(if (and imenu--index-alist (imenu--in-alist word imenu--index-alist))
		(imenu--menubar-select (imenu--in-alist word imenu--index-alist))
	  (find-tag word))))


 (defun find-current-tag-info(tag-name)
   "查找tag所在的文件名和位置"
  (let ((old-buffer (current-buffer))(old-pos (point))(tag-file-name)(tag-position))
	  (save-selected-window
		(find-tag tag-name)
		(setf tag-file-name (buffer-file-name))
		(setf tag-position (point)))
	(switch-to-buffer old-buffer)
	(goto-char old-pos)))

;; 切换h/cpp文件
(defun switch-extension(extension)
  (if (or (string-equal extension "h") (string-equal extension "hpp"))
	  "cpp"
	"h"))
(defun switch-file-name-extension(name)
  (concat (file-name-sans-extension name) "." (switch-extension (file-name-extension name))))
(defun switch-directory (directory)
  (cond ((string-match ".+/src/$" directory)
		 (replace-regexp-in-string "/src/$" "/inc/" directory))
		((string-match ".+/inc/$" directory)
		 (replace-regexp-in-string "/inc/$" "/src/" directory))
		(t directory)))
(defun switch-file-path(file-path)
  (let ((extension (file-name-extension file-path)) (switched-file-path))
	(setf switched-file-path (concat (file-name-sans-extension file-path) "." (switch-extension extension)))
	(when (not (file-exists-p switched-file-path))
	  (setf switched-file-path (concat (switch-directory (file-name-directory switched-file-path)) (file-name-nondirectory switched-file-path))))
	switched-file-path))
(defun switch-head-body(file-path)
  "切换头文件和实现文件"
  (find-file (switch-file-path file-path)))
(defun switch-current-file()
  "当前文件在头/实现文件之间切换"
  (interactive)
  (switch-head-body (buffer-file-name)))

;; 插入追踪日志
(defun cpp-parse-function-arg (arg-string)
  "解析单个函数参数声明中的参数信息,返回'(参数名 . 类型信息)"
  (let ((arg-elements (split-string arg-string)) arg-name arg-type)
	(setq arg-name (car (last arg-elements)))
	(setq arg-type (mapconcat 'identity (butlast arg-elements) " "))
	(cons arg-name arg-type)))
(defun cpp-parse-function-args (args-string)
  "抽取出函数参数声明中的各个参数信息,以'((参数名 . 类型信息)...)的格式返回"
  (let ((arg-string-list (split-string args-string ",")))
	(mapcar #'cpp-parse-function-arg arg-string-list))) ;cpp函数中的参数以,分隔
(defun cpp-get-trace-from-function-arg (args)
  "根据args生成Trace信息"
  (cond ((string-p args)
		 (format "\"%s=[\"+type2string(%s)+\"]\"" args args))
		((list-p args)
		 (mapconcat #'cpp-get-trace-from-function-arg args "+")))) 			;"args=["+type2string(args)+"]"

(defvar cpp-insert-trace-format "LOG_DEBUG(%s);"
  "插入调试信息的模板")
(defun cpp-insert-trace ()
  "插入trace信息"
  (interactive)
  (let (args-string args arg-names)
	(with-temp-buffer
	  (clipboard-yank)
	  (setq args-string (buffer-string)))
	(setq args (cpp-parse-function-args args-string))
	(setq arg-names (mapcar #'car args))
	(insert (format cpp-insert-trace-format (cpp-get-trace-from-function-arg arg-names)))))

(provide 'darksun-cpp-helper)
