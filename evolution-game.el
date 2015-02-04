(require 'cl)
(defvar *width* 100
  "世界边界的宽度")
(defvar *height* 30
  "世界边界的高度")
(defvar *jungle* '(45					;x坐标
				   10					;y坐标
				   10					;宽度
				   10)					;高度
  "丛林的位置与大小,丛林的植物生长速度要快过其他地方")
(defvar *plant-energy* 80
  "每个植物包含的能量")

(defvar *plants* (make-hash-table :test #'equal)
  "全世界植物的分布")

(defun random-plant (left top width height)
  "在指定区域内随机生长一棵植物"
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
	(setf (gethash pos *plants*) t)))

(defun add-plants()
  "在全世界范围内生长两颗植物"
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))

(defstruct animal
  x										;动物所在的坐标
  y
  energy								;动物所拥有的能量,表示动物还能存活的天数
  dir									;动物面向哪个方向,从0到7
  genes									;每个动物拥有8个基因,表示往哪个方向移动的权重
  )

(defvar *animals*
  (list (make-animal :x (ash *width* -1)
					 :y (ash *height* -1)
					 :energy 1000
					 :dir 0
					 :genes (loop repeat 8
								  collect (1+ (random 10)))))
  "动物列表")

(defun move (animal)
  "animal根据dir的方向向前走一步,并消耗自己的能量"
  (let ((dir (animal-dir animal))
		(x (animal-x animal))
		(y (animal-y animal)))
	(setf (animal-x animal) (mod (+ x (cond ((and (>= dir 2) (< dir 5)) 1)
											((or (= dir 1) (= dir 5)) 0)
											(t -1))
									*width*)
								 *width*))
	(setf (animal-y animal) (mod (+ y (cond ((and (>= dir 0) (< dir 3)) -1)
											((and (>= dir 4) (< dir 7)) 1)
											(t 0))
									*height*)
								 *height*))
	(decf (animal-energy animal))))

(defun turn(animal)
  "animal转向"
  (let ((x (random (apply #'+ (animal-genes animal)))))
	(cl-labels ((angle (genes x)			;labels定义一个临时函数
					(let ((xnu (- x (car genes))))
					  (if (< xnu 0)
						  0
						(1+ (angle (cdr genes) xnu))))))
	  (setf (animal-dir animal)
			(mod (+ (animal-dir animal) (angle (animal-genes animal) x))
				  8)))))


(defun eat (animal)
  "若动物所在位置有植物,则动物吃掉植物获取能量"
  (let ((pos (cons (animal-x animal) (animal-y animal))))
	(when (gethash pos *plants*)
	  (incf (animal-energy animal) *plant-energy*)
	  (remhash pos *plants*))))

(defvar *reproduction-energy* 200
  "生育下一代的所需的能量")

(defun reproduce (animal)
  "animal生育下一代,但可能产生基因突变"
  (let ((e (animal-energy animal)))
	(when (>= e *reproduction-energy*)
	  (setf (animal-energy animal) (ash e -1))
	  (let ((animal-nu (copy-animal animal))
			(genes     (copy-list (animal-genes animal)))
			(mutation  (random 8)))
		(setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1)))
		(setf (animal-genes animal-nu) genes)
		(push animal-nu *animals*)))))

(defun update-world ()
  "模拟一天"
  (setf *animals* (remove-if (lambda (animal)
							   (<= (animal-energy animal) 0))
							 *animals*))
  (mapc (lambda (animal)
		  (turn animal)
		  (move animal)
		  (eat animal)
		  (reproduce animal))
		*animals*)
  (add-plants))

(defun fresh-line()
  (princ "\n"))
(defun draw-world ()
  "根据植物和动物的分布,画出整个世界"
  (loop for y
		below *height*
		do (progn
			 (fresh-line)
			 (princ "|")
			 (loop for x
				   below *width*
				   do (princ (cond ((some (lambda (animal)
											(and (= (animal-x animal) x)
												 (= (animal-y animal) y)))
										  *animals*)
									"M")
								   ((gethash (cons x y) *plants*) "*")
								   (t " "))))
			 (princ "|"))))

(defun evolution ()
  "模拟控制界面"
  (interactive)
  (draw-world)
  (fresh-line)
  (let ((str (read-string "")))
	(cond ((equal str "quit") nil)
		  (t (let ((x (string-to-number str)))
			   (if (> x 0)
				   (loop for i
						 below x
						 do (update-world)
						 if (zerop (mod i 1000))
						 do (princ "."))
				 (update-world))
			   (evolution))))))



(evolution)
