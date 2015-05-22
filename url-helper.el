(defun url-get-title-from-html(url)
  "从http url中获取title"
  (let ((url-buffer (url-retrieve-synchronously url))
		url-content
		url-title)
	(with-current-buffer url-buffer
	  (setq url-content (libxml-parse-html-region (point-min) (point-max)))
	  (flet ((save-url-title-fn (cont)
								(dolist (sub cont)
								  (when (eq (car sub) 'text)
									(setq url-title (cdr sub))))))
		(let ((shr-external-rendering-functions '((title . save-url-title-fn))))
		  (shr-insert-document url-content))))
	(kill-buffer url-buffer)
	url-title))
(provide 'url-helper)
