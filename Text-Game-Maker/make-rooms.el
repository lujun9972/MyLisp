(add-to-list 'load-path (pwd))
(defvar display-fn #'message
  "显示信息的函数")
(require 'room-maker)
(map-init "room-description.el" "room-map.ini")
(require 'inventory-maker)
(inventorys-init "inventory-config.el")
(require 'creature-maker)
(creatures-init "creature-config.el")
(require 'action)

(defvar objects-with-cake-hidden '(枕头 碗 辣椒 窗户))
(setq objects-with-cake-hidden '(枕头 碗 辣椒 窗户))
(dolist (symbol objects-with-cake-hidden)
  (setf (member-watch-trigger (get-inventory-by-symbol symbol))
		(lambda ()
		  (when (eq symbol (car objects-with-cake-hidden))
			(take-effect-to-creature myself '(蛋糕 . 1))
			(message "恭喜你发现一枚蛋糕!")
			(cond ((eq symbol '碗)
				   (message "下一个蛋糕的谜面是:置之脑后"))
				  ((eq symbol '窗户)
				   (message "下一个蛋糕的谜面是:大四方,小四方,没有他,闷得慌")))
			(setq objects-with-cake-hidden (remove symbol objects-with-cake-hidden))))))
(watch '辣椒)
(watch)
(move right)
(move right)
(move left)
(get '辣椒)
(describe myself)
(use '辣椒)
(describe myself)

(add-inventory-to-creature myself '辣椒)
(drop '辣椒)
(watch '碗)
(describe myself)
