(defun make-complete-filter-function (process end-output-regex)
  "创建一个filter function,该function会等待output完全读取后才调用process的原filter function进行操作

可以从process的output属性中取到output信息.
当output匹配end-output-regex时,会设置process的output-finished属性为t"
  (lexical-let ((end-output-regex end-output-regex)
				(originally-filter-process (process-filter process)))
	(lambda (process output)
	  (let* ((last-output (process-get process 'output))
			 (output (concat last-output output)))
		(process-put process 'output output)
		(when (string-match-p  end-output-regex output)	;output已经完全读出
		  (funcall originally-filter-process process output))))))

(defun get-process-complete-output (process end-output-regex)
  "获取process的完整output"
  (let (output)
	(while (not (string-match-p end-output-regex (process-get process 'output)))
	  (accept-process-output process nil nil t))
	(setf output (process-get process 'output))
	(process-put process 'output "")
	output))
(provide 'darksun-process-helper)
