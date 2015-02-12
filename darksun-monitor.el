(require 'notifications)

(defun dbus-avaliable-p ()
	"判断Emacs是否编译时支持D-Bus"
	(featurep 'dbusbind))

(defun make-connect-by-plink (remote usr pwd)
  "通过plink建立与remote的远程连接"
  (let* ((connect-name (format "plink-%s@%s" usr remote))
		(connect-buffer connect-name)
		(monitor-process))
	(start-process connect-name connect-buffer "plink" "-l" usr "-pw" pwd remote)))

(defun execute-monitor-command (cmd &optional process)
  "执行监控命令,会自动在监控命令后面添加回车符"
  (let ((command (concat cmd "\n"))
		(process (or process (get-buffer-process (current-buffer)))))
	(process-send-string process command)))

(defun make-output-handler (reaction-rules)
  "创建filter-function

该filter-function根据handler-rules的规则来匹配后续动作.

handler-rules的格式为'((match1 . action1) (match2 . action2)...)

当process的output匹配matchN时,执行actionN命令"
  (lexical-let ((rules reaction-rules))
	(lambda (process output)
	  
	  (let* ((rule (assoc-if (lambda (match)
							   (or (eq match t)
								   (string-match-p match output)))
							 rules))
			 (action (cdr rule)))
		  (internal-default-process-filter process output)
			;; 若dbus可用,则使用notification通知
			(when (dbus-avaliable-p)
			  (notifications-notify :title (process-name process)
									:body output))
			;; 执行action动作
			(cond ((stringp action)
				   (execute-monitor-command action process))
				  ((functionp action)
				   (execute-monitor-command (funcall action  output) process)))))))

(defun start-monitor-process (command reaction-rules &optional time-out process)
  "向process发起监控命令,并根据handler-rules来根据输出执行相应的action"
  (let* ((time-out (or time-out 2))
		 (process (or process (get-buffer-process (current-buffer)))))
	(set-process-filter process (make-output-handler reaction-rules))
	(execute-monitor-command command process)
	(accept-process-output process time-out nil t)))

(start-monitor-process "df|grep cnaps2" '(("8.%" . "echo do clean job") ("9.%" . "echo warnning clean job")) 10 a)


(setq a (make-connect-by-plink "10.8.6.10" "cnaps2" "123456"))

(process-filter a)



