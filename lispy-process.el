(defun lispy-process-default-filter (proc &rest objs)
  "lispy-process的默认filter"
  (unless (buffer-live-p (process-buffer proc))
    (set-process-buffer proc (get-buffer-create (process-name proc)))) ; 若没有buffer，则使用proc name作为buffer名称。 但有可能存在刚好该buffer为dead buffer的情况
  (when (buffer-live-p (process-buffer proc))
	(with-current-buffer (process-buffer proc)
	  (let ((moving (= (point) (process-mark proc))))
		(save-excursion
		  ;; Insert the text, advancing the process marker.
		  (goto-char (process-mark proc))
		  (insert (prin1-to-string objs))
		  (set-marker (process-mark proc) (point)))
		(if moving (goto-char (process-mark proc)))))))

(defun make-lispy-network-process (&rest args)
  "类似`make-network-process'但使用lisp object作为传输对象

filter function的函数签名应该为(process &rest objs) "
  (lexical-let* ((ori-filter-fn (or  (plist-get args :filter)
									 #'lispy-process-default-filter))
				 (store-msg-property (gensym)))
	(plist-put args :filter
			   (lambda (process msg)
				 (let ((content (process-get process store-msg-property))
					   result obj)
				   (setq content (concat content msg))
				   (while (setq result (ignore-errors (read-from-string content)))
					 (setq content (substring content (cdr result)))
					 (setq obj (car result))
					 (apply ori-filter-fn process obj))
				   (process-put process store-msg-property content))))
	(apply #'make-network-process args)))

(defun lispy-process-send (process &rest objs)
  "类似`process-send-string' 但发送的是lisp object"
  (process-send-string process (prin1-to-string objs)))

(defun lispy-process-send-wait (process wait-flag &rest objs)
  "类似`lispy-process-send' 但会等待回应
其中会设置process的'WAIT属性为`wait-flag. 并等待回应函数将'WAIT属性清为nil"
  (process-put process 'WAIT wait-flag)
  (process-send-string process (prin1-to-string objs))
  (while (process-get process 'WAIT)
	(accept-process-output process 0.05)))

(defmacro lispy-process-wait (wait-clause &rest body)
  ""
  (let (wait-var wait-val)
	(cond ((symbolp wait-clause)
		   (setq wait-var wait-clause
				 wait-val t))
		  ((consp wait-clause)
		   (setq wait-var (car wait-clause)
				 wait-val (cdr wait-clause)))
		  (t (error "unknown wait-clause format")))
	(cond ((symbolp wait-var)
		   `(progn 
			  (set ,wait-var ,wait-val)
			  ,@body
			  (while ,wait-var
				(sit-for 0.05))))
		  ((processp wait-var)
		   `(progn
			  (process-put  ,wait-var 'WAIT ,wait-val)
			  ,@body
			  (while (process-get  ,wait-var 'WAIT)
				(accept-process-output ,wait-var 0.05)))))))

;; (defmacro lispy-process-wait (wait-flag &rest bodys)
;;   ""
;;   )

(defun set-lispy-process-filter (process filter)
  "类似`set-filter-filter' 但是`filter'的函数参数应该为(process &rest objs) "
  (lexical-let* ((ori-filter-fn filter)
				 (store-msg-property (gensym)))
	(set-process-filter process
						(lambda (process msg)
						  (let ((content (process-get process store-msg-property))
								result obj)
							(setq content (concat content msg))
							(while (setq result (ignore-errors (read-from-string content)))
							  (setq content (substring content (cdr result)))
							  (setq obj (car result))
							  (apply ori-filter-fn process obj))
							(process-put process store-msg-property content))))))

(defun read-from-lispy-process (process)
  "从lispy process buffer中读取lisp object. 若成功则返回object,并将读取的内容从process buffer中移除,否则返回表示错误的字符串。由于理论上lispy process是以list格式来传送对象的，因此可以用返回结果是否为字符串来判断读取是否失败"
  (condition-case err 
      (let ((buf (process-buffer process))
            obj)
        (with-current-buffer buf
          (setq obj (ignore-errors  (read (current-buffer))))
          (if obj
              (delete-region (point-min) (point))
            (goto-char (point-min))))
        obj)
    (error (format "%s" err))))


(defun read-from-lispy-process-wait (process)
  "从process buffer中读取lisp object. 若成功则返回object,并将读取的内容从process buffer中移除,否则阻塞一直到成功为止"
  (let (obj)
	(while (not (or (eq 'closed (process-status process))
					(equal "(end-of-file)" (setq obj (read-from-process process)))))
	  (accept-process-output process 0.1))
	obj))
(provide 'lispy-process)
