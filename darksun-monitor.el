(require 'cl)
(require 'notifications)

(defstruct monitor exam-cmd
		   reaction-rules)

(defun monitor-process-p (process)
  "判断process是否为monitor process

若某process的monitors不为nil时,为monitor process"
  (process-get process 'monitors))

(defun dbus-avaliable-p ()
	"判断Emacs是否编译时支持D-Bus"
	(featurep 'dbusbind))

(defun make-connect-by-plink (remote usr pwd)
  "通过plink建立与remote的远程连接"
  (let* ((connect-name (format "plink-%s@%s" usr remote))
		(connect-buffer connect-name))
	(start-process connect-name connect-buffer "plink" "-l" usr "-pw" pwd remote)))

(defun make-connect-by-ssh (remote usr pwd)
  "通过ssh建立与remote的远程连接"
  (let* ((connect-name (format "ssh-%s@%s" usr remote))
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

(defun process-output-finished-p (process)
  "process的output是否已经完全读取完毕"

  (process-get process 'output-finished))

(defun modify-process-output-finished (process flag)
  "更改process的output-finished标志"

  (process-put process 'output-finished flag))

(defun execute-monitor-command (cmd &optional process)
  "执行监控命令,会自动在监控命令后面添加回车符,并返回命令结果"
  (let ((command (concat cmd "\n"))
		(process (or process (get-buffer-process (current-buffer))))
		output)
	(process-send-string process command)
	(modify-process-output-finished process nil)
	(while (not (process-output-finished-p process))
	  (accept-process-output process nil nil t))
	(setf output (process-get process 'output))
	(process-put process 'output "")
	output))

(defun monitor-filter-function (process output)
  "一直等到output完全读完才输出到buffer中,并把process的output-finished标志设为t

output是否读完,根据process的output-end-line来标识"
  (let* ((last-output (process-get process 'output))
		 (output (concat last-output output)))
	(process-put process 'output output)
	(when (string-match-p  (process-get process 'output-end-line)
						   output)		;output已经完全读出
	  (modify-process-output-finished process t)
	  (internal-default-process-filter process output))))

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
	  ;; (when (dbus-avaliable-p)
	  ;; 	(notifications-notify :title (process-name process)
	  ;; 						  :body output))
	  ;; 执行action动作
	  (cond ((stringp action)
			 (execute-monitor-command action process))
			((functionp action)
			 (execute-monitor-command (funcall action  output) process))
			(t
			 (error (format "error action type[%s]" (type-of action))))))))

(defun start-monitor-process (remote usr pwd &optional wait-time)
  "创建一个process用于执行monitor

该函数返回连接到usr@remote的process,并且其filter-function为`monitor-filter-functiion'"
  (let ((process (setq process (make-or-raise-connect remote usr pwd)))
		(wait-time (or wait-time 3)))
	(process-put process 'output "")
	(while (accept-process-output process wait-time nil t) ;若一段时间内无值,则认为登录进去了,推出循环等待
	  (sit-for 1))
	(set-process-filter process #'monitor-filter-function)
	(cl-labels ((get-last-line (process)
							   "获取process buffer中最后一行的内容"
							   (with-current-buffer (process-buffer process) 
								 (goto-char (point-max))
								 (search-backward-regexp "[\r\n]")
								 (buffer-substring-no-properties (1+ (point)) (point-max)))))
	  (process-put process 'output-end-line (regexp-quote (get-last-line process))))
	process))

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

(setq a (start-monitor-process "localhost" "lujun9972" "7758521"))
(process-get a 'output-last-line)
(setq a (start-monitor-process "10.8.6.10" "cnaps2" "123456"))

(add-process-monitor a 
					 (make-monitor :exam-cmd "df"
								   :reaction-rules '(("8.%" . "echo do clean job")
													 ("9.%" . "echo warnning clean job"))))

(add-process-monitor a
				(make-monitor :exam-cmd "sleep 10;df "
							  :reaction-rules '(("%" . "echo disk if full"))))
