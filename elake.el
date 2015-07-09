#! emacs --script

(eval-when-compile
  (defvar elake-task-relationship (make-hash-table)
	"存放task之间的依赖关系"))

(defmacro elake-task (task-symbol prepare-task-list &optional doc-string &rest body)
  "使用elask-task宏来定义task"
  ;; 存储依赖关系到elask-task-relationship中
  (puthash task-symbol prepare-task-list elake-task-relationship)
  ;; 定义名为task-symbol的函数,以doc-string为函数说明,body为函数体
  `(defun ,task-symbol ()
	 ,doc-string
	 ,@body)
  )

(defun elake--execute-task (task-symbol)
  "运行`task-symbol'标识的任务,会预先运行它的prepare-tasks"
  (let ((prepare-task-list (gethash task-symbol elake-task-relationship)))
	(when prepare-task-list
	  (cond ((sequencep prepare-task-list)
			 (mapc #'elake--execute-task prepare-task-list))
			((symbolp prepare-task-list)
			 (elake--execute-task prepare-task-list))
			(t (error "错误的依赖类型:%s" (type-of prepare-task-list)))))
	(if (functionp task-symbol)
		(funcall task-symbol)
	  (error "未定义的任务:%s" task-symbol))))

(defmacro elake-execute-task (task-symbol)
  `(elake--execute-task ',task-symbol))

(load "~/myLisp/elakefile.el")

(dolist (task (mapcar #'intern (nthcdr 3 command-line-args)))
  (elake--execute-task task))
