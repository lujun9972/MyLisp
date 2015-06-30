(defvar display-fn #'message
  "显示信息的函数")
(require 'room-maker)
(require 'inventory-maker)
(require 'equipment-maker)
;; action functions

;; 移动到各rooms的命令
(defconst up 0)
(defconst right 1)
(defconst down 2)
(defconst left 3)
(defun move(directory)
  "往`directory'方向移动"
  (when (symbolp directory)
	(setq (cdr (assoc directory '((0 . up) (1 . right) (2 . down) (3 . left))))))
  (let ((new-room-symbol (nth directory (beyond-rooms (room-symbol currect-room) room-map))))
	(if new-room-symbol
		(progn
		  ;; 触发离开事件
		  (when (room-out-trigger currect-room)
			(funcall (room-out-trigger currect-room)))
		  (setq currect-room (get-room-by-symbol new-room-symbol))
		  ;; 触发进入事件
		  (when (room-in-trigger currect-room)
			(funcall (room-in-trigger currect-room)))
		  (funcall display-fn (describe currect-room)))
	  (funcall display-fn "那里没有路"))))

(defun watch (&optional object)
  "查看物品/周围环境"
  (cond ((stringp object)
		 (setq object (intern object))))
  (if (or (null object)
		  (member object (room-inventory currect-room))
		  (member object (room-creature currect-room)))
	  (let ((object (or (unless object currect-room)
						(get-room-by-symbol object)
						(get-inventory-by-symbol object))))
		(when (and (slot-exists-p object 'watch-fn)
				   (slot-boundp object 'watch-fn)
				   (slot-value object 'watch-fn))
		  (funcall (slot-value object 'watch-fn)))
		(describe object))
	(funcall display-fn "不能查看该物品")))

(defun get (inventory)
  "获取物品/周围环境"
  (cond ((stringp inventory)
		 (setq inventory (intern inventory))))
  (let (object)
	(if (member inventory (room-inventory currect-room))
		(let ((object (get-inventory-by-symbol inventory)))
		  (when (and (slot-exists-p object 'get-fn)
					 (slot-boundp object 'get-fn)
					 (slot-value object 'get-fn))
			(funcall (slot-value object 'get-fn)))
		  (add-inventory-to-creature myself inventory)
		  (remove-inventory-from-room currect-room inventory))
	  (funcall display-fn "没有该物品"))))
