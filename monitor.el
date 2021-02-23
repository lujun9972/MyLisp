(require 'cl-lib)
(require 'process-helper)


(cl-defstruct monitor exam-cmd
		   reaction-rules)

(defun monitor-process-p (process)
  "判断process是否为monitor process

若某process的monitors不为nil时,为monitor process"
  (process-get process 'monitors))


(defun execute-monitor-command (cmd &optional process)
  "执行监控命令,会自动在监控命令后面添加回车符,并返回命令结果"
  (let ((command (concat cmd "\n"))
		(process (or process (get-buffer-process (current-buffer))))
		output)
	(process-send-string process command)
	(get-process-complete-output process (process-get process 'end-output-regex))))

(defun reaction (process output reaction-rules)
  "根据handler-rules的规则来匹配后续动作.

reaction-rules的格式为由(match . action)组成的alist

当process的output匹配matchN时,执行actionN命令:若action为字符串,则往process发送action命令,否则action为函数,它接收output作为参数,并返回要发送給process的命令字符串"
  (let* ((rule (assoc-if (lambda (match)
						   (or (eq match t)
							   (string-match-p match output)))
						 reaction-rules))
		 (action (cdr rule)))
	(when rule
	  ;; 若dbus可用,则使用notification通知
	  ;; (when (featurep 'dbusbind)
	  ;; 	(require 'notifications)
	  ;; 	(notifications-notify :title (process-name process)
	  ;; 						  :body output))
	  ;; 执行action动作
	  (cond ((stringp action)
			 (execute-monitor-command action process))
			((functionp action)
			 (execute-monitor-command (funcall action  output) process))
			(t
			 (error (format "error action type[%s]" (type-of action))))))))

(defun do-monitor (process  monitor  )
  "向process发起监控命令,并根据reaction-rules来根据输出执行相应的action"
  (let ((exam-cmd (monitor-exam-cmd monitor))
		(reaction-rules (monitor-reaction-rules monitor))
		(output ""))
	(process-put process 'current-monitor monitor)
	(setf output (execute-monitor-command exam-cmd process))
	(reaction process output reaction-rules)))

(defun do-monitors (process  &rest monitors)
  (dolist (monitor monitors)
	(do-monitor process monitor)))

(defun add-process-monitor (process monitor)
  "为process增加monitor"
  (cond ((stringp process)
		 (setq process (get-process process)))
		((bufferp process)
		 (setq process (get-buffer-process process))))
  (if (processp process)
	  (pushnew monitor (process-get process 'monitors))
	(error "process表示的进程不存在!")))

(defun active-all-processes-monitors ()
  "激活*process-monitors-map*中所有的process,让他们执行对应的monitors"
  (interactive)
  (mapc (lambda (process)
			 (apply #'do-monitors process  (process-get process 'monitors)))
		   (remove-if-not #'monitor-process-p (process-list))))

(provide 'monitor)
