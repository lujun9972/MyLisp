#! emacs --script

(require 'cl)
(eval-when-compile
  (defvar elake-task-relationship (make-hash-table)
	"存放task之间的依赖关系")
  (defvar elake-executed-task nil
	"已经执行过的task,不要重新执行")
  (defvar elake--ns nil
	"命名空间"))

;; 定义namespace
(defmacro elake-namespace (ns &rest body)
  (declare (indent 2))
  (let ((elake--ns ns))
	`(progn
	   ,@(mapcar #'macroexpand body))
  ))
;; 定义task
(defmacro elake-task (task prepare-task-list &optional doc-string &rest body)
  "使用elask-task宏来定义task"
  (declare (indent defun) (doc-string 3))
  ;; 统一prepare-task-list为list格式
  (unless (listp prepare-task-list)
	(setq prepare-task-list (list prepare-task-list)))
  (when elake--ns
	(setq task (intern (format "%s:%s" elake--ns task)))
	(setq prepare-task-list (mapcar (lambda (task)
									  (if (elake--file-task-p task)
										  task
										(intern (format "%s:%s" elake--ns task)))) prepare-task-list)))
  ;; 存储依赖关系到elask-task-relationship中
  (puthash task prepare-task-list elake-task-relationship)
  ;; 定义名为task-symbol的函数,以doc-string为函数说明,body为函数体
	`(defun ,task ($< $@)
	   ,doc-string
	   ,@body)
  )
;; command line args处理函数
(defun command-line-get-args-to-next-option ()
  "用于获取直到下一个option为止的所有command line args,会将获取的command line args移出`command-line-args-left'变量"
  (let* ((next-option-position (or (cl-position-if (lambda (arg)
													 (string-prefix-p "-" arg)) command-line-args-left)
								   (length command-line-args-left)))
		 (args-to-next-option (subseq command-line-args-left 0 next-option-position)))
	(setq command-line-args-left (nthcdr next-option-position command-line-args-left))
	args-to-next-option))
;; 显示任务说明
(defun elake--show-task-documentation (task)
  "显示`task'指定任务的说明"
  (when (stringp task)
	(setq task (intern task)))
  (message "%s:%s" task (documentation task)))

(defun elake--show-tasks-documentation (&rest tasks)
  "显示`tasks'指定任务的说明"
  (when (null tasks)
	(require 'subr-x)
	(setq tasks (hash-table-keys elake-task-relationship)))
  (mapc #'elake--show-task-documentation tasks))

(defun elake-show-tasks-documentation (option)
  "显示指定任务的说明文档"
  (apply 'elake--show-tasks-documentation (command-line-get-args-to-next-option)))

;; 显示task的准备条件
(defun elake--get-task-preparations (task)
  "返回`task'的依赖任务"
  (gethash task elake-task-relationship))


(defun elake--show-task-preparations (task)
  "显示`task'指定任务的说明"
  (when (stringp task)
	(setq task (intern task)))
  (message "%s:%s" task (elake--get-task-preparations task)))

(defun elake--show-tasks-preparations (&rest tasks)
  "显示`tasks'指定任务的说明"
  (when (null tasks)
	(require 'subr-x)
	(setq tasks (hash-table-keys elake-task-relationship)))
  (mapc #'elake--show-task-preparations tasks))

(defun elake-show-tasks-preparations (option)
  "显示指定任务的依赖任务"
  (apply 'elake--show-tasks-preparations (mapcar #'read (command-line-get-args-to-next-option))))

;; 帮助的函数
(defun elake--show-option-help (option)
  "根据`command-switch-alist'显示`option'的帮助信息"
  (let* ((command-switch (assoc option command-switch-alist))
		 (option (car command-switch))
		 (fn (cdr command-switch))
		 (help (documentation fn)))
	(message "%s:\t%s" option help)))

(defun elake--show-options-help (&rest options)
  "根据`command-switch-alist'显示`options'中各个option的帮助信息"
  (when (null options)
	(setq options (mapcar #'car command-switch-alist)))
  (mapc #'elake--show-option-help options))

(defun elake-show-help (option)
  "显示帮助信息"
  (apply 'elake--show-options-help (command-line-get-args-to-next-option)))

;; 执行task函数
(defun elake--file-task-p (task)
  "判断`task'是否为file类型的任务,这种类型的任务采取make的方式处理,需要判断依赖文件和目标文件的更新时间. 若是file类型的任务,则返回对应的file路径

file类型的任务以`file#'开头"
  (let ((task-name (format "%s" task)))
	(when (string-prefix-p "file#" task-name)
	  (replace-regexp-in-string "file#" "" task-name))))
(defalias 'elake--get-path-from-file-task 'elake--file-task-p
  "若`task'为file类型的task,则返回对应的file path")

(defun elake--phony-task-p (task)
  "判断`task'是否为phony类型的任务,这种类型的任务采取ant的方式处理,单纯的执行被依赖的任务

非file类型的任务就是phony类型的任务"
  (not (elake--file-task-p task)))

(defun elake--task-executed-p (task)
  "判断`task'是否已经执行"
  (member task elake-executed-task))

(defun elake--need-to-execute-task-p (task)
  "判断`task'是否需要执行"
  (let ((preparations (elake--get-task-preparations task)))
	(cond ((and (elake--phony-task-p task)
				(elake--task-executed-p task))
		   nil)								;phony任务已执行过,则不再执行
		  ((and (elake--file-task-p task)
				(file-exists-p (elake--file-task-p task)) ;file任务的file已存在
				;; (cl-notany #'elake--need-to-execute-task-p preparations) ;且不存在 "未处理的依赖任务或不存在的依赖文件"
				(cl-notany (lambda (preparation-file)
							 (file-newer-than-file-p preparation-file (elake--file-task-p task)))
						   (remove nil (mapcar #'elake--file-task-p preparations)))) ;且不存在依赖文件比目标文件更新的情况
		   nil)							;才不用执行
		  (t t))))						;否则需要执行

(defun elake--execute-task (task)
  "运行`task'标识的任务,会预先运行它的prepare-tasks"
  ;; (when (stringp task)
  ;; 	(setq task (intern task)))
  (let ((prepare-task-list (elake--get-task-preparations task)))
	;; 执行预备条件
	(when prepare-task-list
	  (cond ((sequencep prepare-task-list)
			 (mapc #'elake--execute-task prepare-task-list))
			(t (error "错误的依赖类型:%s" (type-of prepare-task-list)))))
	  (when (elake--need-to-execute-task-p task )
	  (if (functionp task)
		  (progn
			(push task elake-executed-task)
			(funcall task task prepare-task-list))
	 	(error "未定义的任务:%s" task)))))

(defmacro elake-execute-task (task)
  (if task
	  `(elake--execute-task (quote ,task))
	  `(elake--execute-task ,(read argi))))

;; 加载elakefile文件
(add-to-list 'load-path (file-name-directory load-file-name))
(load "elakefile" nil t)

;; 设置参数处理函数
(add-to-list 'command-switch-alist '("--task" . elake-show-tasks-documentation))
(add-to-list 'command-switch-alist '("-p" . elake-show-tasks-preparations))
(add-to-list 'command-switch-alist '("--preparations" . elake-show-tasks-preparations))
(add-to-list 'command-switch-alist '("-h" . elake-show-help))
;; (add-to-list 'command-line-functions 'elake-execute-task)
(add-to-list 'command-line-functions (lambda ()
									   (elake--execute-task (read argi))))
