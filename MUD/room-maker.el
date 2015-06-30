(defvar display-fn #'message
  "显示信息的函数")
(defvar rooms-alist nil
  "symbol与room对象的映射")

(defun get-room-by-symbol (symbol)
  "根据symbol获取room对象"
  (cdr (assoc symbol rooms-alist)))

;; 定义Room类
(defclass Room nil
  ((symbol :initform (intern (format "room-%s" (length rooms-alist))) :initarg :symbol :accessor room-symbol :documentation "ROOM标志")
   (description :initarg :description :accessor room-description :documentation "ROOM描述")
   (inventory :initform nil :initarg :inventory :accessor room-inventory :documentation "ROOM中所有的物品")
   (creature :initform nil :initarg :creature :accessor room-creature :documentation "ROOM中所拥有的生物")
   (in-trigger :initform nil :initarg :in-trigger :accessor room-in-trigger :documentation "进入该ROOM后触发的事件")
   (out-trigger :initform nil :initarg :out-trigger :accessor room-out-trigger :documentation "离开该ROOM后触发的事件")
   ))

(defmethod describe ((room Room))
  "输出room的描述"
  (cl-multiple-value-bind (up-room right-room down-room left-room)  (beyond-rooms (room-symbol room) room-map)
	(format "这里是%s\n%s\n物品列表:%s\n生物列表:%s\n附近的rooms: up:%s right:%s down:%s left:%s" (room-symbol room) (room-description room) (room-inventory room) (room-creature room) up-room right-room down-room left-room)))

;; 创建room列表的方法
(defun build-room (room-entity)
  "根据`text'创建room,并将room存入`rooms-alist'中"
  (cl-multiple-value-bind (symbol description inventory equipment creature) room-entity
	(cons symbol (make-instance Room :symbol symbol :description description :inventory inventory :equipment equipment :creature creature))))

(defun build-rooms(room-config-file)
  "根据`room-config-file'中的配置信息创建各个room"
  (let ((room-entities (read-from-whole-string (file-content room-config-file))))
	(mapcar #'build-room room-entities)))

(defun remove-inventory-from-room (room inventory)
  ""
  (setf (room-inventory room) (remove inventory (room-inventory room))))

(defun add-inventory-to-room (room inventory)
  ""
  (push (room-inventory room) inventory))

(defun kill-creature-from-room (room inventory)
  ""
  (setf (room-creature room) (remove inventory (room-creature room))))
;; 将各room组装成地图的方法
(defvar room-map nil
  "room的地图")

(defun build-room-map(room-map-config-file)
  "根据`room-map-config-file'中的配置信息创建地图"
  (let* ((file-lines (split-string (file-content room-map-config-file) "[\r\n]")))
	(mapcar (lambda(line)
			  (mapcar #'intern (split-string line)))
			file-lines)))

;; 
(defun get-room-position (room-symbol room-map)
  "从`room-map'中取出`room-symbol'标识的room的坐标"
  (let* ((x (cl-position-if (lambda(x-rooms)
							  (member room-symbol x-rooms)) room-map))
		 (y (cl-position room-symbol (nth x room-map))))
	(list x y)))

;; 
(defun beyond-rooms (room-symbol room-map)
  "根据room-map取与room-symbol相邻的room列表"
  (cl-multiple-value-bind (x y) (get-room-position room-symbol room-map)
	(let ((height (length room-map))
		  (width (length (car room-map)))
		  up down left right)
	  (setq up (if (= x 0)
				   nil
				 (nth y (nth (1- x) room-map))))
	  (setq down (if (= x (1- height))
					 nil
				   (nth y (nth (1+ x) room-map))))
	  (setq left (if (= y 0)
					 nil
				   (nth (1- y) (nth x room-map))))
	  (setq right (if (= y (1- width))
					  nil
					(nth (1+ y) (nth x room-map))))
	  (list up right down left))))

;; 定义初始化函数
(defvar currect-room nil				;
  "当前所处的room对象")

(defun map-init(room-config-file room-map-config-file)
  "初始化函数,生成room对象,组装map"
  (setq rooms-alist (build-rooms room-config-file))
  (setq room-map (build-room-map room-map-config-file))
  (setq currect-room (get-room-by-symbol (caar rooms-alist))))

(provide 'room-maker)

