(add-to-list 'load-path (file-name-directory (buffer-file-name)))
(defvar display-fn #'tg-mprinc
  "显示信息的函数")
(require 'room-maker)
(map-init "room-description.el" "room-map.ini")
(require 'inventory-maker)
(inventorys-init "inventory-config.el")
(require 'creature-maker)
(creatures-init "creature-config.el")
(require 'action)
(require 'tg-mode)

(defvar objects-with-cake-hidden '(梅花 枕头 沙发 辣椒 窗户))
(dolist (symbol objects-with-cake-hidden)
  (setf (member-watch-trigger (get-inventory-by-symbol symbol))
		(lambda ()
		  (when (eq symbol (car objects-with-cake-hidden))
			(add-inventory-to-creature myself '蛋糕)
			(funcall display-fn "恭喜你发现一枚蛋糕!")
			(cond ((eq symbol '梅花)
				   (funcall display-fn "下一个蛋糕的谜面是:火车从湖南省省会发出"))
				  ((eq symbol '沙发)
				   (funcall display-fn "下一个蛋糕的谜面是:置之脑后"))
				  ((eq symbol '枕头)
				   (funcall display-fn "下一个蛋糕的谜面是:大四方,小四方,没有他,闷得慌"))
				  ((eq symbol '窗户)
				   (funcall display-fn "下一个蛋糕的谜面是:小时绿葱葱，老来红彤彤。剥开皮来看，一堆白虫虫")))
			(setq objects-with-cake-hidden (remove symbol objects-with-cake-hidden))))))

(defun tg-history()
  "game的背景说明"
  (apply display-fn "
%s君,很遗憾第告诉你,你亲爱的Emacs被暗黑大魔王抢走.

而你被打魔王封印在了一间小房子里,无法出去.

你需要在小房子中找到5个蛋糕,召唤食神才能破除魔王封印.

(这是一个猜迷游戏,我会告诉你一个迷面,其谜底对应房间里的一样东西,对该东西执行watch操作既能得到蛋糕,并获得下一个谜面)"))


(defun text-game ()
  "Switch to *Text Game* buffer and start game."
  (interactive)
  (require 'tg-mode)
  (switch-to-buffer "*Text Game*")
  (tg-history)
  (tg-help)
  (tg-mode)
  (setq tg-over-p nil)
  (tg-messages))
