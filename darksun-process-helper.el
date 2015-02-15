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
		  (process-put process 'output "")
		  (funcall filter-function process output))))))

(defun make-output-filter-function (filter-function)
  "创建一个filter function,该function会执行原filter-function的功能,但同时会把output添加到process的output属性中去"
  (lexical-let ((filter-function filter-function))
	(lambda (process output)
	  (let* ((last-output (process-get process 'output)))
		(process-put process 'output (concat last-output output))
		(funcall filter-function process output)))))

(defun get-process-complete-output (process end-regex-or-time)
  "获取process的完整output"
  (let ((originally-filter-function (process-filter process))
		output)
	(set-process-filter process (make-output-filter-function originally-filter-function))
	(cond ((stringp end-regex-or-time)
		   (while (not (string-match-p end-regex-or-time (process-get process 'output))) ;若获取到的值符合end-regex,则表示读取到了完整的output
			 (accept-process-output process nil nil t)))
		  ((numberp end-regex-or-time)
		   (while (accept-process-output process end-regex-or-time nil t) ;若一段时间内无值,则认为已经读取了完整的output,退出循环等待
			 )))
	(setf output (process-get process 'output))
	(process-put process 'output "")
	(set-process-filter process originally-filter-function)
	output))

;;;; 创建远程连接
(defun make-connect-by-plink (remote usr pwd)
  "通过plink建立与remote的远程连接"
  (let* ((connect-name (format "%s@%s" usr remote))
		 (connect-buffer connect-name))
	(start-process connect-name connect-buffer "plink" "-l" usr "-pw" pwd remote)))

(defun make-connect-by-ssh (remote usr pwd)
  "通过ssh建立与remote的远程连接"
  (let* ((connect-name (format "%s@%s" usr remote))
		 (connect-buffer connect-name)
		 (process (start-process connect-name connect-buffer "ssh" "-l" usr remote)))
	(accept-process-output process nil nil t)
	(process-send-string process (concat pwd "\n") )
	process))

(defun make-connect (remote usr pwd)
  (cond ((executable-find "plink")
		 (make-connect-by-plink remote usr pwd))
		((executable-find "ssh")
		 (make-connect-by-ssh remote usr pwd))
		(t (error "没找到建立远程连接的程序"))))

(defun make-or-raise-connect (remote usr pwd)
  "若已经有usr@remote的连接,则直接返回该连接process,否则新建一个连接process"
  (let ((connect-name (format "%s@%s" usr remote)))
	(or (get-process connect-name)
		(make-connect remote usr pwd))))

(defun start-connect-process (remote usr pwd &optional wait-time)
  "创建一个与远程服务器相连的连接process

该函数返回连接到usr@remote的process,并且该process的end-output-regex记录了命令提示符的值,可以使用命令提示符来标识一个命令是否执行完毕"
  (let ((process (make-or-raise-connect remote usr pwd))
		(wait-time (or wait-time 3)))
	(process-put process 'output "")
	(get-process-complete-output process wait-time) ;确定登录完成了
	(cl-labels ((get-last-line (process)
							   "获取process buffer中最后一行的内容"
							   (with-current-buffer (process-buffer process) 
								 (goto-char (point-max))
								 (search-backward-regexp "[\r\n]")
								 (buffer-substring-no-properties (1+ (point)) (point-max)))))
	  (process-put process 'end-output-regex (regexp-quote (get-last-line process))))
	process))

(provide 'darksun-process-helper)


