(require 'notifications)

(defstruct monitor exam-cmd
		   reaction-rules)

(defcustom *process-monitors-map* (make-hash-table)
  "存放每个process对应的monitor")

(defcustom *time-out* 10
  "每个monitor执行后获取output的超时时间")

(defun dbus-avaliable-p ()
	"判断Emacs是否编译时支持D-Bus"
	(featurep 'dbusbind))

(defun make-connect-by-plink (remote usr pwd)
  "通过plink建立与remote的远程连接"
  (let* ((connect-name (format "plink-%s@%s" usr remote))
		(connect-buffer connect-name)
		(monitor-process))
	(start-process connect-name connect-buffer "plink" "-l" usr "-pw" pwd remote)))

(defun make-or-raise-connect-by-plink (remote usr pwd)
  "若已经有usr@remote的连接,则直接返回该连接process,否则新建一个连接process"
  (let ((connect-name (format "plink-%s@%s" usr remote)))
	(or (get-process connect-name)
		(make-connect-by-plink remote usr pwd))))

(defun execute-monitor-command (cmd &optional process)
  "执行监控命令,会自动在监控命令后面添加回车符"
  (let ((command (concat cmd "\n"))
		(process (or process (get-buffer-process (current-buffer)))))
	(process-send-string process command)))

(defun monitor-filter-function (process output)
  "该filter-function根据handler-rules的规则来匹配后续动作.

handler-rules的格式为由(match . action)组成的alist

当process的output匹配matchN时,执行actionN命令:若action为字符串,则往process发送action命令,否则action为函数,它接收output作为参数,并返回要发送給process的命令字符串"
  (internal-default-process-filter process output)
  (let* ((reaction-rules (monitor-reaction-rules (process-get process 'current-monitor)))
		 (rule (assoc-if (lambda (match)
						   (or (eq match t)
							   (string-match-p match output)))
						 reaction-rules))
		 (action (cdr rule)))
	(when rule
	  ;; 若dbus可用,则使用notification通知
	  ;; (when (dbus-avaliable-p)
	  ;; 	(notifications-notify :title (process-name process)
	  ;; 						  :body output))
	  ;; 执行action动作
	  (cond ((stringp action)
			 (execute-monitor-command action process))
			((functionp action)
			 (execute-monitor-command (funcall action  output) process))
			(t (message "error action type[%s]" (type-of action)))))))

(defun start-monitor-process (remote usr pwd)
  "创建一个process用于执行monitor

当该process没有存放于`*process-monitors-map*'中作为key时,则加入到`*process-monitors-map*'中

该函数返回连接到usr@remote的process,并且其filter-function为`monitor-filter-functiion'"
  (let (process)
	(setq process (make-or-raise-connect-by-plink remote usr pwd))
	(unless (gethash process *process-monitors-map*)
	  (setf (gethash process *process-monitors-map*) nil))
	(set-process-filter process #'monitor-filter-function)
	process))

(defun do-monitor (process time-out monitor  )
  "向process发起监控命令,并根据reaction-rules来根据输出执行相应的action"
  (let ((exam-cmd (monitor-exam-cmd monitor))
		(reaction-rules (monitor-reaction-rules monitor)))
	(process-put process 'current-monitor monitor)
	(execute-monitor-command exam-cmd process)
	(accept-process-output process time-out nil t)))

(defun do-monitors (process time-out &rest monitors)
  (dolist (monitor monitors)
	(do-monitor process time-out monitor)))

(defun add-process-monitor (process monitor)
  "为process增加monitor"
  (cond ((stringp process)
		 (setq process (get-process process)))
		((bufferp process)
		 (setq process (get-buffer-process process))))
  (if (processp process)
	  (pushnew monitor (gethash process *process-monitors-map* ))
	(error "process表示的进程不存在!")))

(defun active-all-processes-monitors ()
  "激活*process-monitors-map*中所有的process,让他们执行对应的monitors"
  (interactive)
  (maphash (lambda (process monitors)
			 (apply #'do-monitors process *time-out* monitors))
		   *process-monitors-map*))


(setq a (start-monitor-process "10.8.6.10" "cnaps2" "123456"))

(add-process-monitor a 
					 (make-monitor :exam-cmd "df|grep cnaps2"
								   :reaction-rules '(("8.%" . "echo do clean job")
													 ("9.%" . "echo warnning clean job"))))

(add-process-monitor "plink-cnaps2@10.8.6.10" 
				(make-monitor :exam-cmd "df |grep 100%"
							  :reaction-rules '(("." . "echo disk if full"))))
