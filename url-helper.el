(require 'cl-lib)
(defun url-get-content-from-url(url)
  "获取 `url'的内容"
  (let ((url-buffer (url-retrieve-synchronously url))
		url-content)
	(with-current-buffer url-buffer
	  (goto-char (point-min))
	  (search-forward-regexp "^$")
	  (unless (= (point) (point-max))
		(setq url-content (buffer-substring-no-properties (+ (point) 1) (point-max)))))
	(kill-buffer url-buffer)
	url-content))

(defun url-get-content-from-html-async (url)
  ""
  (url-retrieve url (lambda (status)
					  (setq url-content (libxml-parse-xml-region (point-min) (point-max))))))
(defun url-get-content-from-html(url)
  "从http `url'中获取经过`libxml-parse-html-region'解析的内容"
  (let ((url-buffer (url-retrieve-synchronously url))
		url-content)
	(with-current-buffer url-buffer
	  (search-forward-regexp "^$")
	  (setq url-content (libxml-parse-html-region (point) (point-max))))
	(kill-buffer url-buffer)
	url-content))

(defun url-download-from-url (url &optional filename what-if-already-exists keep-time preserve-uid-gid)
  "下载url所指文件,`filename'默认为`url'所指的文件名. `what-if-already-exist'表示当下载的文件已存在时该如何处理,'ignore表示跳过不下载,nil表示报错,其他表示覆盖"
  (unless filename
	(setq filename (car (last (split-string url "/")))))
  (unless (and (eq what-if-already-exist 'ignore)
			   (file-exists-p filename))
	(url-copy-file url filename what-if-already-exists keep-time preserve-uid-gid)))

(defun url-http-post (url args)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method "POST")
		(url-request-extra-headers
		 (cons '("Content-Type" . "application/x-www-form-urlencoded")
               url-request-extra-headers))
		(url-request-data
		 (mapconcat (lambda (arg)
					  (concat (url-hexify-string (format "%s" (car arg)))
							  "="
							  (url-hexify-string (format "%s" (cdr arg)))))
					args
					"&")))
	(url-retrieve url 'url-kill-url-buffer)))

(defun url-kill-url-buffer (status)
  "Kill the buffer returned by `url-retrieve'."
  (kill-buffer (current-buffer)))

(defun url-switch-to-url-buffer (status)
  "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
  (switch-to-buffer (current-buffer)))

(provide 'url-helper)
