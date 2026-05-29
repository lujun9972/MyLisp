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

(defcustom my/annotate-target-buffer "*scratch*"
  "注解的目标 buffer 名称。"
  :type 'string
  :group 'convenience)

(defun my/annotate--next-number (buffer)
  "计算 BUFFER 中末尾连续编号列表的下一个编号。

从 buffer 末尾向前扫描，跳过空行和缩进行（注解行），找到连续的有序列表项，
返回最大编号 + 1。若没有编号列表则返回 1。"
  (with-current-buffer buffer
    (let ((max-num 0))
      (save-excursion
        (goto-char (point-max))
        (catch 'done
          (while t
            (when (/= (forward-line -1) 0) (throw 'done nil))
            (cond
             ((looking-at "^\\([0-9]+\\)\\. ")
              (setq max-num (max max-num (string-to-number (match-string 1)))))
             ((looking-at-p "^\\s-*$") nil)
             ((looking-at-p "^\\s-+") nil)
             (t (throw 'done nil))))))
      (1+ max-num))))

(defun my/annotate-region (start end)
  "将选中的文字追加到目标 buffer，用有序列表编号，并在下一行添加注解。

目标 buffer 由 `my/annotate-target-buffer' 决定。
使用 C-u 前缀参数可以临时指定目标 buffer。

输出格式：
  N. 原文内容
     注解内容（缩进对齐到编号后）"
  (interactive "r")
  (unless (use-region-p)
    (user-error "请先选中区域"))
  (let* ((target-buf-name (if current-prefix-arg
                              (read-buffer "目标 buffer: " my/annotate-target-buffer)
                            my/annotate-target-buffer))
         (target-buf (get-buffer-create target-buf-name))
         (text (buffer-substring-no-properties start end))
         (annotation (read-string "注解: "))
         (next-num (my/annotate--next-number target-buf))
         (num-str (number-to-string next-num))
         (indent (make-string (+ (length num-str) 2) ?\s)))
    (with-current-buffer target-buf
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert (format "%d. %s\n" next-num text))
      (unless (string-empty-p annotation)
        (insert (format "%s%s\n" indent annotation))))))

(provide 'edit-helper)
