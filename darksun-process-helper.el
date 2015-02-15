(defun make-complete-filter-function (filter-function end-output-regex)
  "创建一个filter function,该function会等待output完全读取后才调用原filter function进行操作

可以从process的output属性中取到output信息. "
  (lexical-let ((end-output-regex end-output-regex)
				(filter-function filter-function))
	(lambda (process output)
	  (let* ((last-output (process-get process 'output))
			 (output (concat last-output output)))
		(process-put process 'output output)
		(when (string-match-p  end-output-regex output)	;output已经完全读出
		  (funcall filter-function process output))))))

(defun get-process-complete-output (process end-output-regex)
  "获取process的完整output"
  (let ((originally-filter-function (process-filter process))
		output)
	(set-process-filter process (make-complete-filter-function originally-filter-function end-output-regex))
	(while (not (string-match-p end-output-regex (process-get process 'output)))
	  (accept-process-output process nil nil t))
	(setf output (process-get process 'output))
	(process-put process 'output "")
	(set-process-filter process originally-filter-function)
	output))

(provide 'darksun-process-helper)


