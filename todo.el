(require 'cl-lib)
(require 'subr-x)

(defgroup el-todo nil
  "a todo command line tool used in eshell")

(defvar *todo-tasks* nil
  "任务列表")

(defcustom todo--default-task-pri 5
  "任务默认优先级")

(cl-defun todo--gen-task-id ()
  "generate a new task-id"
  (gensym))

(cl-defstruct task
  (id (todo--gen-task-id))
  (desc (read-string "请输入任务描述: "))
  (pri todo--default-task-pri)
  (tags nil)
  (create-time (current-time-string))
  (schedule-time nil)
  (deadline-time nil)
  (done-time nil)
  (doing-periods nil))

(cl-defun todo (cmd &rest args)
  "The main function"
  (apply (intern (format "todo-%s" (downcase cmd)))
		 (mapcar (lambda (arg)
				   (if (stringp arg)
					   (cond ((string-prefix-p ":" arg)
							  (intern arg))
							 ((string-match-p "^[[:digit:]]+$" arg)
							  (string-to-number arg))
							 (t
							  arg))
					 arg)) args)))


;; todo add task-description
(cl-defun todo-add (description &key pri tags sd dl)
  "add a new task"
  (push (make-task :desc description
				   :pri (or pri todo--default-task-pri)
				   :tags tags
				   :schedule-time sd
				   :deadline-time dl) *todo-tasks*))

;; todo show
(cl-defun todo--filter (&key id desc pri tag)
  "filter and list tasks"
  (let ((tasks *todo-tasks*))
	(when id
	  (setq tasks (remove-if-not (lambda (task)
								   (eq id (task-id task)))
								 tasks)))
	(when desc
	  (setq tasks (remove-if-not (lambda (task)
								   (string-match-p desc (task-desc task)))
								 tasks)))
	(when pri
	  (setq tasks (remove-if-not (lambda (task)
								   (<= pri (task-pri task)))
								 tasks)))
	(when tag
	  (setq tasks (remove-if-not (lambda (task)
								   (member tag (task-tags task)))
								 tasks)))
	tasks))

(cl-defun todo--show-task (task)
   (message "%s: %s :pri %s :tags %s"
		   (task-id task)
		   (task-desc task)
		   (task-pri task)
		   (task-tags task)))

(cl-defun todo-show (&key desc pri tag)
  "filter and list tasks"
  (let ((tasks (todo--filter :desc desc :pri pri :tag tag)))
	(string-join (mapcar #'todo--show-task tasks) "\n")))

;; todo edit task-id task-description
(cl-defun todo--find-task-by-id (id)
  "find task by `id'. If no task found,throw an error"
  (let ((task (car (todo--filter :id id))))
	(unless task
	  (error "not found No.%s task" id))
	task))

(defmacro todo--with-task (id &rest body)
  (declare (indent 2))
  `(let ((THE-TASK (todo--find-task-by-id ,id)))
	,@body))

(cl-defun todo-edit (id desc)
  "edit task's description"
  (todo--with-task id
	(setf (task-desc THE-TASK) desc)))

;; todo done task-id
(cl-defun todo-done (id)
  "mark a task done,and remember the finish time"
  (todo--with-task id 
	  (setf (task-done-time THE-TASK) (current-time-string))))

;; todo do task-id
(cl-defun todo-do (id)
  "mark doing a task"
  (todo--with-task id
	  (push (list (current-time-string)) (task-doing-periods THE-TASK))))

;; todo pause task-id
(cl-defun todo-pause (id)
  "mark doing a task"
  (todo--with-task id
	  (append (car (task-doing-periods THE-TASK)) (current-time-string))))

;; todo save
(defcustom todo-save-file "todo-file.save"
  "the file used to save tasks")

(cl-defun todo-save ()
  "save todo tasks"
  (with-temp-file todo-save-file
	(insert (prin1 *todo-tasks*))))

;; todo load
(cl-defun todo-load ()
  "load todo tasks"
  (with-temp-buffer
	(insert-file-contents todo-save-file)
	(setq *todo-tasks* (read-from-whole-string (buffer-string)))))

;; todo pull server user pwd

;; todo push server
