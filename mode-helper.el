(defun try-enable-mode (mode-function)
  "若能开启该mode,则开启它"
  (if (functionp mode-function)
	  (progn
		(funcall mode-function 1)
		t)
	nil))

(defun try-disable-mode (mode-function)
  "若能关闭该mode,则关闭它"
  (if (functionp mode-function)
	  (progn
		(funcall mode-function -1)
		t)
	nil))

(defun enable-prefer-mode (&rest mode-functions)
  "从多个类似的mode中选择启用其中一个mode

该函数从前往后遍历mode-functions. 开启存在的第一个mode,其他mode全部关闭"
  (mapcar 'try-disable-mode mode-functions)
  (some 'try-enable-mode mode-functions))

;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
	(add-to-list 'auto-mode-alist (cons pattern mode))))

(provide 'mode-helper)
