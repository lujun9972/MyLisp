
(defun org-gamification-point-to-score()
  "定位到积分行"
  (interactive)
  (goto-char (point-min))
  (when (not (search-forward-regexp "^#\\+SCORES: " nil t))
    (goto-char (point-max))
    (if (search-backward-regexp "^#\\+" nil t)
        (progn
          (end-of-line)
          (newline))
      (progn
      (goto-char (point-min))
      (newline)
      (previous-line)))
    (beginning-of-line)
    (insert "#+SCORES: ")))

(defun org-gamification-get-score()
  "获取当前累计的积分数量"
  (save-excursion
    (org-gamification-point-to-score)
    (string-to-int (buffer-substring-no-properties (point) (line-end-position)))))

(defun org-gamification-update-score(&optional newScore)
  "更新当前积分"
  (save-excursion
    (org-gamification-point-to-score)
    (insert (number-to-string newScore))
    (insert " points")
    (backward-word)
    (kill-line)))

(defun org-gamification-add-score (score)
  "增加指定积分"
  (save-excursion
    (let (newScore)
      (setq newScore (+ score (org-gamification-get-score)))
      (org-gamification-update-score newScore))))

(defun org-gamification-remove-score-able-p (score)
  "判断是否能够扣减指定分数"
  (> (org-gamification-get-score) score))

(defun org-gamification-remove-score (score)
  "减少指定积分"
  (save-excursion
    (if (org-gamification-remove-score-able-p score)
        (org-gamification-update-score (- (org-gamification-get-score) score))
      (message "积分不足"))))

(defun org-gamification-get-entry-reward-score ()
  "获取完成entry该获得的积分"
  (save-excursion
    ;;(org-back-to-heading t)
    (if (org-entry-get nil "REWARD" t)
        (string-to-int (org-entry-get nil "REWARD" t))
      (+ 10 (/ (org-get-priority (thing-at-point 'line)) 100)))))

(defun org-gamification-point-to-reward ()
  "跳转到REWARD headline"
  (interactive)
  (let (reward-headline-pos)
    (setq reward-headline-pos (org-find-exact-headline-in-buffer "REWARDS" nil t))
    (when (null reward-headline-pos)
      (goto-char (point-max))
      (newline)
      (beginning-of-line)
      (insert "* REWARDS")
      (beginning-of-line)
      (setq reward-headline-pos (point))
      )
    (goto-char reward-headline-pos))
  )

(defun org-gamification-add-reward ()
  "增加奖励物品,需要用积分购买"
  (interactive)
  (save-excursion
    (let (reward-name reward-price)
      (org-gamification-point-to-reward)
      (end-of-line)
      (setq reward-name (read-string "请输入奖品名称: "))
      (org-insert-subheading nil)
      (insert reward-name)
      (setq reward-price (read-string "请输入奖品价格(整数): "))
      (org-set-property "PRICE" (int-to-string (string-to-int reward-price)))
      (org-set-tags-to ":REWARD:")
      ))
  )

(defun org-gamification-buy-reward-able-p()
  "判断是否能够购买奖励物品"
  (if (org-entry-get nil "PRICE" t)
      (org-gamification-remove-score-able-p (string-to-int (org-entry-get nil "PRICE" t)))
    (progn
      (message "该奖励没有设置PRICE")
      nil)))

(defun org-gamification-buy-reward ()
  "购买奖励物品,会减少积分"
  (when (org-gamification-buy-reward-able-p)
    (org-gamification-remove-score (string-to-int (org-entry-get nil "PRICE" t)))))

(defun org-gamification-sell-reward ()
  "售卖奖励物品,会增加积分"
  (if (org-entry-get nil "PRICE" t)
      (org-gamification-add-score (string-to-int (org-entry-get nil "PRICE" t)))
    (message "该奖励没有设置PRICE"))
  )

(defun org-gamification-reward-p ()
  "判断该entry是否属于奖励"
  (save-excursion
      (org-back-to-heading)
      (find "REWARD" (org-get-tags) :test 'string=)))

(defun org-gamification-entry-trigger (task-plist)
  "完成事项,增加积分"
  (let (from-state to-state )
    (setq from-state (plist-get task-plist :from))
    (setq to-state (plist-get task-plist :to))
    (save-excursion
      (when (and (member to-state org-done-keywords) ( or (member from-state org-not-done-keywords) (null from-state)))
        (if (org-gamification-reward-p)
            (org-gamification-buy-reward)
          (org-gamification-add-score (org-gamification-get-entry-reward-score))))
      (when (and (or (member to-state org-not-done-keywords) (null to-state)) (member from-state org-done-keywords))
        (if (org-gamification-reward-p)
            (org-gamification-sell-reward)
          (org-gamification-remove-score (org-gamification-get-entry-reward-score)))))))

(defun org-gamification-entry-blocker (task-plist)
  "若动作会将积分变成负数,则不能进行该动作"
  (let (from-state to-state )
    (setq from-state (plist-get task-plist :from))
    (setq to-state (plist-get task-plist :to))
    (save-excursion
      (cond ((and (member to-state org-done-keywords) ( or (member from-state org-not-done-keywords) (null from-state)))
             (if (org-gamification-reward-p)
                 (org-gamification-buy-reward-able-p)
               t))
            ((and (or (member to-state org-not-done-keywords) (null to-state)) (member from-state org-done-keywords))
             (if (not (org-gamification-reward-p))
                 (org-gamification-remove-score-able-p (org-gamification-get-entry-reward-score))
               t))
            (t t)))))

(defun org-gamification-init()
  "org游戏化初始化函数
     初始化积分为0
     初始化游戏的hook
     "
  (org-gamification-update-score 0)
  (org-gamification-start))

(defun org-gamification-start ()
  "初始化游戏的hook"
  (interactive)
  (add-to-list 'org-trigger-hook 'org-gamification-entry-trigger)
  (add-to-list 'org-blocker-hook 'org-gamification-entry-blocker))

(defun org-gamification-end ()
  "结束游戏"
  (interactive)
  (setq org-trigger-hook (remove 'org-gamification-entry-trigger org-trigger-hook))
  (setq org-blocker-hook (remove 'org-gamification-entry-blocker org-blocker-hook)))
