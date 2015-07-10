#! emacs --script

(eval-when-compile
  (defvar elake-task-relationship (make-hash-table)
	"存放task之间的依赖关系")
  (defvar elake-executed-task nil
	"已经执行过的task,不要重新执行"))

;; 定义task
(defmacro elake-task (task-symbol prepare-task-list &optional doc-string &rest body)
  "使用elask-task宏来定义task"
  (declare (indent defun))
  ;; 存储依赖关系到elask-task-relationship中
  (puthash task-symbol prepare-task-list elake-task-relationship)
  ;; 定义名为task-symbol的函数,以doc-string为函数说明,body为函数体
  `(defun ,task-symbol ()
	 ,doc-string
	 ,@body)
  )

;; 执行task函数
(defun elake--execute-task (task-symbol)
  "运行`task-symbol'标识的任务,会预先运行它的prepare-tasks"
  (when (stringp task-symbol)
	(setq task-symbol (intern task-symbol)))
  (let ((prepare-task-list (gethash task-symbol elake-task-relationship)))
	(when prepare-task-list
	  (cond ((sequencep prepare-task-list)
			 (mapc #'elake--execute-task prepare-task-list))
			((symbolp prepare-task-list)
			 (elake--execute-task prepare-task-list))
			(t (error "错误的依赖类型:%s" (type-of prepare-task-list)))))
	(if (and (functionp task-symbol))
		(unless (memq task-symbol elake-executed-task)
		  (push task-symbol elake-executed-task)
		  (funcall task-symbol))
	  (error "未定义的任务:%s" task-symbol))))

(defun elake-execute-task ()
  (elake--execute-task argi))

;; 显示任务说明
(defun elake--task-documentation (&rest task-symbols)
  "显示`task-symbol'指定任务的说明"
  (when (null task-symbols)
	(require 'subr-x)
	(setq task-symbols (hash-table-keys elake-task-relationship)))
  (mapc (lambda (task-symbol)
		  (when (stringp task-symbol)
			(setq task-symbol (intern task-symbol)))
		  (message "%s:%s" task-symbol (documentation task-symbol))) task-symbols))

(defun elake-task-documentation (option)
  (apply 'elake--task-documentation command-line-args-left)
  (setq command-line-args-left nil))

;; 显示task的准备条件
(defun elake--task-preparation (&rest task-symbols)
  "显示`task-symbol'指定任务的说明"
  (when (null task-symbols)
	(require 'subr-x)
	(setq task-symbols (hash-table-keys elake-task-relationship)))
  (mapc (lambda (task-symbol)
		  (when (stringp task-symbol)
			(setq task-symbol (intern task-symbol)))
		  (message "%s:%s" task-symbol (gethash task-symbol elake-task-relationship))) task-symbols))

(defun elake-task-preparation (option)
  (apply 'elake--task-preparation command-line-args-left)
  (setq command-line-args-left nil))

;; 加载elakefile文件
(add-to-list 'load-path (file-name-directory load-file-name))
(load "elakefile" nil t)

;; 设置参数处理函数
(add-to-list 'command-switch-alist '("--task" . elake-task-documentation))
(add-to-list 'command-switch-alist '("-p" . elake-task-preparation))
(add-to-list 'command-line-functions 'elake-execute-task)
