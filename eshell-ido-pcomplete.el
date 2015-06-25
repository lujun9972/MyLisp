(require 'eshell)

(defun eshell-ido-pcomplete--input-lisp-p()
  "判断eshell的输入是否为lisp

以(开头的输入被认为是lisp"
  (= ?\( (elt (eshell-get-old-input) 0)))

(defun eshell-ido-pcomplete--incomplete-input-lisp-function-p()
  "判断待补全的输入是否为函数,若为函数,则返回待补全的输入,否则返回nil"
  (when (eshell-ido-pcomplete--input-lisp-p)
	(save-excursion
	  (let ((cur-point (point))
			(delim-point (search-backward-regexp "[ ()']")))
		(if (= ?\( (char-after delim-point))
			(buffer-substring-no-properties (+ 1 delim-point) cur-point)
		  nil)))))

(defun eshell-ido-pcomplete--special-form-or-function-p (object)
  "判断object是function或special-form"
  (or (functionp object)
	  (special-form-p object)))

(defun eshell-ido-pcomplete--incomplete-input-lisp-symbol-p()
  "判断待补全的输入是否为symbol,若为symbol,则返回待补全的输入,否则返回nil"
  (when (eshell-ido-pcomplete--input-lisp-p)
	(save-excursion
	  (let ((cur-point (point))
			(delim-point (search-backward-regexp "[ ()']")))
		(if (= ?\' (char-after delim-point))
			(buffer-substring-no-properties (+ 1 delim-point) cur-point)
		  nil)))))

(defun eshell-ido-pcomplete--incomplete-input-lisp-variable-p()
  "判断待补全的输入是否为变量,若为变量,则返回待补全的输入,否则返回nil"
  (when (eshell-ido-pcomplete--input-lisp-p)
	(save-excursion
	  (let ((cur-point (point))
			(delim-point (search-backward-regexp "[ ()']")))
		(if (find (char-after delim-point) " )")
			(buffer-substring-no-properties (+ 1 delim-point) cur-point)
		  nil)))))

(defun eshell-ido-pcomplete--pcomplete-completions()
  "用于eshell-ido-pcomplete中生产补全内容的函数,会设置变量`pcomplete-stub'为待补全的内容,并返回补全的后选项"
  (let (completions)
	(cond ((setq pcomplete-stub (eshell-ido-pcomplete--incomplete-input-lisp-function-p))
		   (mapatoms (lambda (x)
					   (when (eshell-ido-pcomplete--special-form-or-function-p x)
						 (push x completions))))
		   completions)
		  ((setq pcomplete-stub (eshell-ido-pcomplete--incomplete-input-lisp-variable-p))
		   (mapatoms (lambda (x)
					   (unless (eshell-ido-pcomplete--special-form-or-function-p x)
						 (push x completions))))
		   completions)
		  ((setq pcomplete-stub (eshell-ido-pcomplete--incomplete-input-lisp-symbol-p))
		   (mapatoms (lambda (x)
					   (push x completions)))
		   completions)
		  (t (pcomplete-completions)))))

(defun eshell-ido-pcomplete ()
  "使用ido作为eshell的pcomplete方法"
  (interactive)
  ;; @ To simplify completion function logic, the tag `pcompleted' may be thrown with a value of nil in order to abort the function.  It means that there were no completions available.
  (catch 'pcompleted
	(let* (completion-result
		   (completions (eshell-ido-pcomplete--pcomplete-completions))
		   (candidates (all-completions pcomplete-stub completions))
		   (pcomplete-stub (replace-regexp-in-string ".*[\/]" "" pcomplete-stub))
		   )
	  (cond ((null candidates)
			 (error "没有匹配项"))
			((= 1 (length candidates))
			 (setq completion-result (car candidates)))
			(t (setq completion-result (ido-completing-read ": " candidates nil nil pcomplete-stub))))
	  (delete-char (- (length pcomplete-stub)))
	  (insert completion-result))))

(when eshell-mode-map
  (define-key eshell-mode-map (kbd "<tab>") 'eshell-ido-pcomplete))

(provide 'eshell-ido-pcomplete)
