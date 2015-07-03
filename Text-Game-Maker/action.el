(defvar display-fn #'message
  "显示信息的函数")
(defvar tg-valid-actions ()
  "允许执行的命令")

(add-to-list 'load-path "~/myLisp/Text-Game-Maker")
(require 'room-maker)
(require 'inventory-maker)
;; action functions
(defmacro tg-defaction (action args doc-string &rest body)
  `(progn 
	 (add-to-list 'tg-valid-actions ',action)
	 (defun ,action ,args
	   ,doc-string
	   ,@body)))
;; 移动到各rooms的命令
(tg-defaction tg-move(directory)
			  "使用'move up/right/down/left'往`directory'方向移动"
			  (when (stringp directory)
				(setq directory (intern directory)))
			  (setq directory (cdr (assoc directory '((up . 0) (right . 1) (down . 2) (left . 3)))))
			  (unless directory
				(throw 'exception "未知的方向"))
			  (let ((new-room-symbol (nth directory (beyond-rooms (member-symbol currect-room) room-map))))
				(unless new-room-symbol
				  (throw 'exception "那里没有路"))
				;; 触发离开事件
				(when (member-out-trigger currect-room)
				  (funcall (member-out-trigger currect-room)))
				(setq currect-room (get-room-by-symbol new-room-symbol))
				;; 触发进入事件
				(when (member-in-trigger currect-room)
				  (funcall (member-in-trigger currect-room)))
				(tg-display (describe currect-room))))

(tg-defaction tg-watch (&optional symbol)
			  "使用'watch'查看周围环境
使用'watch 物品'查看指定物品"
			  (cond ((stringp symbol)
					 (setq symbol (intern symbol))))
			  (unless (or (null symbol)
						  (inventory-exist-in-room-p currect-room symbol)
						  (creature-exist-in-room-p currect-room symbol))
				(throw 'exception (format "房间中没有%s" symbol )))
			  (let ((object (or (unless symbol currect-room)
								(get-room-by-symbol symbol)
								(get-inventory-by-symbol symbol))))
				(when (and (slot-exists-p object 'watch-trigger)
						   (slot-boundp object 'watch-trigger)
						   (slot-value object 'watch-trigger))
				  (funcall (slot-value object 'watch-trigger)))
				(describe object)))

(tg-defaction tg-take (inventory)
			  "使用'take 物品'获取ROOM中的物品"
			  (cond ((stringp inventory)
					 (setq inventory (intern inventory))))
			  (unless (inventory-exist-in-room-p currect-room inventory)
				(throw 'exception (format "房间中没有%s" inventory)))
			  (let ((object (get-inventory-by-symbol inventory)))
				(when (and (slot-exists-p object 'take-trigger)
						   (slot-boundp object 'take-trigger)
						   (slot-value object 'take-trigger))
				  (funcall (slot-value object 'take-trigger)))
				(add-inventory-to-creature myself inventory)
				(remove-inventory-from-room currect-room inventory)))

(tg-defaction tg-drop (inventory)
			  "使用'drop 物品'丢弃身上的物品"
			  (cond ((stringp inventory)
					 (setq inventory (intern inventory))))
			  (unless (inventory-exist-in-creature-p myself inventory)
				(throw 'exception (format "身上没有%s" inventory)))
			  (let ((object (get-inventory-by-symbol inventory)))
				(when (and (slot-exists-p object 'drop-trigger)
						   (slot-boundp object 'drop-trigger)
						   (slot-value object 'drop-trigger))
				  (funcall (slot-value object 'drop-trigger)))
				(remove-inventory-from-creature myself inventory)
				(add-inventory-to-room currect-room inventory)))

(tg-defaction tg-use (inventory)
			  "使用'use 物品'消耗自己随身携带的inventory"
			  (cond ((stringp inventory)
					 (setq inventory (intern inventory))))
			  (unless (inventory-exist-in-creature-p myself inventory)
				(throw 'exception (format "未携带%s" inventory)))
			  (unless (inventory-usable-p inventory)
				(throw 'exception (format "%s不可使用" inventory)))
			  (let ((object (get-inventory-by-symbol inventory)))
				(when (and (slot-exists-p object 'use-trigger)
						   (slot-boundp object 'use-trigger)
						   (slot-value object 'use-trigger))
				  (funcall (slot-value object 'use-trigger)))
				(take-effects-to-creature myself (member-effects object))
				(remove-inventory-from-creature myself inventory)))

(tg-defaction tg-wear (equipment)
			  "使用'wear 物品'装备自己随身携带的equipment"
			  (cond ((stringp equipment)
					 (setq equipment (intern equipment))))
			  (unless (equipment-exist-in-creature-p myself equipment)
				(throw 'exception (format "未携带%s" equipment)))
			  (unless (inventory-wearable-p equipment)
				(throw 'exception (format "%s不可使用" equipment)))
			  (let ((object (get-inventory-by-symbol equipment)))
				(when (and (slot-exists-p object 'use-trigger)
						   (slot-boundp object 'use-trigger)
						   (slot-value object 'use-trigger))
				  (funcall (slot-value object 'use-trigger)))
				(take-effects-to-creature myself (member-effects object))
				(remove-inventory-from-creature myself equipment)
				(add-inventory-to-creature creature myself equipment)
				(tg-display (format "您装备了%s" equipment))))

(tg-defaction tg-status(&optional useless)
			  "使用'status'查看自己的状态"
			  (tg-display (describe myself)))

(tg-defaction tg-help (&rest actions)
			  "使用'help'查看各action说明
使用'help action'查看指定action的说明"
			  (unless actions
				(setq actions tg-valid-actions))
			  (dolist (action actions)
				(when (stringp action)
				  (setq action (intern (format "tg-%s" action))))
				(tg-display (documentation action))))
(tg-defaction tg-quit()
			  "使用'quit'退出游戏"
			  (setq tg-over-p t))
(provide 'action)
