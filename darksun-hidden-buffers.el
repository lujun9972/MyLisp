(require 'cl)
(defgroup hidden-buffers nil
  "隐藏无用的buffer")

(defcustom hidden-buffer-names 
	'("*tramp.+*")
	"要隐藏的buffer的名称列表"
	:group 'hidden-buffers
	:type '(repeat regexp))

(defun judge-unhide-buffer-by-name (buffer)
  "根据名称来隐藏buffer"
  (notany (lambda (reg)
			(string-match reg (buffer-name buffer)))
		  hidden-buffer-names))

(defun set-all-frame-parameter (param value)
  "为所有frame设置属性"
  (mapc (lambda (frame)
		  (set-frame-parameter frame param value)) (frame-list)))

(defun turn-on-hidden-buffer()
  "开启不自动切换到特定buffer上的特性"
  (interactive)
  (set-all-frame-parameter 'buffer-predicate 'judge-unhide-buffer-by-name)
  (push '(buffer-predicate . judge-unhide-buffer-by-name) default-frame-alist))

(defun turn-off-hidden-buffer()
  "关闭不自动切换到特定buffer上的特性"
  (interactive)
  (set-all-frame-parameter 'buffer-predicate nil)
  (let (pos)
	(setq pos (position 'buffer-predicate default-frame-alist))
	(when pos
	  (setf (elt default-frame-alist pos) nil))))
  ;; (assq-delete-all 'buffer-predicate default-frame-alist))

(provide 'darksun-hidden-buffers)













