(defun multi-replace-regexp(&optional start end ignore-prefix)
  "多重替换,需要以alist格式输入替换的对应关系

提供prefix-arg可以指定起始行和结束行"
  (interactive "r")
  (if (and current-prefix-arg
		   (not ignore-prefix))
		(save-excursion
			(goto-line (read-number "请输入起始行:"))
			(setq start (point-at-bol))
			(goto-line (read-number "请输入结束行"))
			(setq end (point-at-eol))

			(multi-replace-string start end t))
	
	(let* ((match-rules (read-minibuffer "请输入替代对应关系,格式为((src1 . des1) (src2 . des2)...)"))
		   (from-regex (string-join (mapcar  (lambda (x)
											   (format "\\(%s\\)" (car x))) match-rules) "\\|"))
		   (start (or start (point-min)))
		   (end (or end (ponit-max))))
	  (save-excursion
		(goto-char start)
		(while (re-search-forward from-regex end t)
		  (replace-match (format "%s" (cdr (assoc-string (match-string 0) match-rules)))))))))

(provide 'edit-helper)
