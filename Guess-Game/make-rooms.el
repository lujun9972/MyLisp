(add-to-list 'load-path (format "%s../Text-Game-Maker" (file-name-directory (buffer-file-name))))
(defvar display-fn #'tg-mprinc
  "显示信息的函数")
(require 'text-game-maker)
(map-init "room-description.el" "room-map.ini")
(inventorys-init "inventory-config.el")
(creatures-init "creature-config.el")

(defvar objects-with-cake-hidden '(梅花 沙发 枕头 窗户 辣椒))
(dolist (symbol objects-with-cake-hidden)
  (setf (member-watch-trigger (get-inventory-by-symbol symbol))
		(lambda ()
		  (when (eq symbol (car objects-with-cake-hidden))
			(add-inventory-to-creature myself '蛋糕)
			(tg-display "恭喜你发现一枚蛋糕!")
			(cond ((eq symbol '梅花)
				   (tg-display "下一个蛋糕的谜面是:火车从湖南省省会发出"))
				  ((eq symbol '沙发)
				   (tg-display "下一个蛋糕的谜面是:置之脑后"))
				  ((eq symbol '枕头)
				   (tg-display "下一个蛋糕的谜面是:大四方,小四方,没有他,闷得慌"))
				  ((eq symbol '窗户)
				   (tg-display "下一个蛋糕的谜面是:小时绿葱葱，老来红彤彤。剥开皮来看，一堆白虫虫")))
			(setq objects-with-cake-hidden (remove symbol objects-with-cake-hidden))))))
;; 设置`外面'的in-trigger,若蛋糕不足,则不能外出,会被随机传送
(setf (member-in-trigger (get-room-by-symbol '门外))
	  (lambda ()
		(let* ((inventory (member-inventory myself))
			   (cake-number (count '蛋糕 inventory))
			   room-symbol)
		  (if (< cake-number 5)
			  (progn
				(tg-display "你的蛋糕不足,无法召唤食神,你被大魔王的封印随机传送到某个房间")
				(setq room-symbol (nth (random 3) '(大厅 厨房 卧室)))
				(setq currect-room (get-room-by-symbol room-symbol)))
			(tg-display "你收集了足够的蛋糕,召唤出的食神破除了大魔王的封印. 恭喜你,过关了...")
			(tg-quit)))))
(defun gg-history()
  "game的背景说明"
  (tg-display (format "
%s君,很遗憾第告诉你,你亲爱的Emacs被暗黑大魔王抢走.

而你被大魔王封印在了一间小房子里,无法出去.

你需要在小房子中找到5个蛋糕,召唤食神才能破除魔王封印.

(这是一个猜迷游戏,我会告诉你一个迷面,其谜底对应房间里的一样东西,对该东西执行watch操作即可得到蛋糕,并获得下一个谜面)

使用help可以查看可以使用的各个命令及说明

那么游戏开始了....

先用watch命令查看一下周边环境吧,你会发现地一个蛋糕位置的线索" (member-symbol myself))))


(defun guess-game ()
  "Switch to *Text Game* buffer and start game."
  (interactive)
  (require 'tg-mode)
  (switch-to-buffer "*Text Game*")
  (setq tg-over-p nil)
  (gg-history)
  (tg-mode)
  (tg-messages))

