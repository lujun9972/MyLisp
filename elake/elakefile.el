;; 可以使用(lexical-let)来模拟块域任务(Block Scoping  Tasks)
(lexical-let ((who "somebody"))
  (elake-task say-hello (wash) 
	"say hello "
	(message "hello %s" who)))
;; 可以通过elake 变量=值的方式給任务传递新变量
(elake-task say-hello-to  nil
	"say hello to "
	(message "hello to %s" who))
;; 可以使用$<表示目标任务,$@表示依赖任务列表
(elake-task wash (file$wash) 
  "wash faces"
  (message "%s,%s" $< $@))
;; file$FILE-PATH格式的任务为文件任务
(elake-task file$wash (file$bowl)
  (shell-command "touch wash"))
(elake-task file$bowl ()
  (shell-command "touch bowl"))
(elake-task go-out (say-hello  wash)
  (message "go out"))
;; 使用(elake-namespace ns &rest body)定义命名空间
(elake-namespace home
	(elake-task purchaseVegetables nil
	  "任务1 -- 买菜"
	  (message  "到沃尔玛去买菜。"))
  (elake-task cook (purchaseVegetables)
	"任务2 -- 做饭"
	(message  "做一顿香喷喷的饭菜。")))


(elake-task laundry nil
  "任务3 -- 洗衣服"
  (message "把所有衣服扔进洗衣机。"))
;; 使用(elake-execute-task task)在任务内执行其他任务
(elake-task today nil
  "今天的任务"
  (elake-execute-task home:cook)
  (elake-execute-task laundry))
