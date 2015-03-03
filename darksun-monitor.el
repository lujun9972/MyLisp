;; 使用说明:
;; 0. 在使用前,请保证系统有plink或ssh
;; 1. 加载monitor库:
;; (require 'darksun-monitor)
;; 2. 创建一个process连接到要监控的远程机器上
;; (setq p1 (start-connect-process "10.8.6.10" "cnaps2" "123456"))
;; 3. 可以创建多个process连接到不同的远程机器上
;; 4. 创建一个monitor,一个monitor由要执行的检测命令,以及根据检测命令的返回结果指定相应回应命令的rule列表组成
;; (setq m1 (make-monitor :exam-cmd "df |grep cnaps2"
;; 							 :reaction-rules '(("[89]?%" . "echo disk is almost full")
;; 											   ("100%" . "echo disk is full! please clean it"))))
;; 5. 使用add-process-monitor将monitor应用到表示远程机器的process上
;; (add-process-monitor p1 m1)
;; 6. 可以为一个process添加多个monitor
;; 7. 执行(active-all-processes-monitors)会执行次所有process中的所有monitor
;; 8. 若想每隔10s钟自动激活一次process中的所有monitor,可以:
;; (setq t1 (run-at-time 0 10 #'active-all-processes-monitors))

(require 'cl)
(require 'darksun-process-helper)


(defstruct monitor exam-cmd
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

handler-rules的格式为由(match . action)组成的alist

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

(provide 'darksun-monitor)
