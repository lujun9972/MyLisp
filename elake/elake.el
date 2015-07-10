#! emacs --script

(require 'cl)
(eval-when-compile
  (defvar elake-task-relationship (make-hash-table)
	"存放task之间的依赖关系")
  (defvar elake-executed-task nil
	"已经执行过的task,不要重新执行"))

;; 定义task
(defmacro elake-task (task prepare-task-list &optional doc-string &rest body)
  "使用elask-task宏来定义task"
  (declare (indent defun))
  ;; 统一prepare-task-list为list格式
  (unless (listp prepare-task-list)
	(setq prepare-task-list (list prepare-task-list)))
  ;; 存储依赖关系到elask-task-relationship中
  (puthash task prepare-task-list elake-task-relationship)
  ;; 定义名为task-symbol的函数,以doc-string为函数说明,body为函数体
	`(defun ,task ($< $@)
	   ,doc-string
	   ,@body)
  )

;; 执行task函数
(defun elake--file-task-p (task)
  "判断`task'是否为file类型的任务,这种类型的任务采取make的方式处理,需要判断依赖文件和目标文件的更新时间. 若是file类型的任务,则返回对应的file路径

file类型的任务以`file:'开头"
  (let ((task-name (format "%s" task)))
	(when (string-prefix-p "file:" task-name)
	  (replace-regexp-in-string "file:" "" task-name))))

(defun elake--phony-task-p (task)
  "判断`task'是否为phony类型的任务,这种类型的任务采取ant的方式处理,单纯的执行被依赖的任务

非file类型的任务就是phony类型的任务"
  (not (elake--file-task-p task)))

(defun elake--task-executed-p (task)
  "判断`task'是否已经执行"
  (member task elake-executed-task))

(defun elake--need-to-execute-task-p (task preparations)
  "根据`preparation'判断`task'是否需要执行"
  (cond ((and (elake--phony-task-p task)
			  (not (elake--task-executed-p task)))
		 t)								;phony任务未执行
		((and (elake--file-task-p task)
			  (not (file-exists-p (elake--file-task-p task)))) 
		 t)								;file任务的file不存在
		((and (elake--file-task-p task)
			  (file-exists-p (elake--file-task-p task))
			  (cl-some (lambda (preparation-file)
						 (or (not (file-exists-p preparation-file))
							 (file-newer-than-file-p preparation-file (elake--file-task-p task))))
					   (remove nil (mapcar #'elake--file-task-p preparations))))
		 t)								;依赖文件不存在,或依赖文件更新了
		(t nil)))						;否则不再执行

(defun elake--execute-task (task)
  "运行`task'标识的任务,会预先运行它的prepare-tasks"
  (when (stringp task)
	(setq task (intern task)))
  (let ((prepare-task-list (gethash task elake-task-relationship)))
	;; 执行预备条件
	(when (elake--need-to-execute-task-p task prepare-task-list)
	  (when prepare-task-list
		(cond ((sequencep prepare-task-list)
			   (mapc #'elake--execute-task prepare-task-list))
			  (t (error "错误的依赖类型:%s" (type-of prepare-task-list)))))
	  (if (functionp task)
		  (progn
			(push task elake-executed-task)
			(funcall task task prepare-task-list))
		(error "未定义的任务:%s" task)))))

(defun elake-execute-task ()
  (elake--execute-task argi))

;; 显示任务说明
(defun elake--task-documentation (&rest tasks)
  "显示`tasks'指定任务的说明"
  (when (null tasks)
	(require 'subr-x)
	(setq tasks (hash-table-keys elake-task-relationship)))
  (mapc (lambda (task)
		  (when (stringp task)
			(setq task (intern task)))
		  (message "%s:%s" task (documentation task))) tasks))

(defun elake-task-documentation (option)
  (apply 'elake--task-documentation command-line-args-left)
  (setq command-line-args-left nil))

;; 显示task的准备条件
(defun elake--task-preparation (&rest tasks)
  "显示`tasks'指定任务的说明"
  (when (null tasks)
	(require 'subr-x)
	(setq tasks (hash-table-keys elake-task-relationship)))
  (mapc (lambda (task)
		  (when (stringp task)
			(setq task (intern task)))
		  (message "%s:%s" task (gethash task elake-task-relationship))) tasks))

(defun elake-task-preparation (option)
  (apply 'elake--task-preparation (mapcar #'read command-line-args-left))
  (setq command-line-args-left nil))

;; 加载elakefile文件
(add-to-list 'load-path (file-name-directory load-file-name))
(load "elakefile" nil t)

;; 设置参数处理函数
(add-to-list 'command-switch-alist '("--task" . elake-task-documentation))
(add-to-list 'command-switch-alist '("-p" . elake-task-preparation))
(add-to-list 'command-line-functions 'elake-execute-task)
