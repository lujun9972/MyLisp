(defvar rooms-alist nil
  "symbol与room对象的映射")

;; 创建ROOM
(defclass Room nil
  ((symbol :initform (intern (format "room-%s" (length rooms-alist))) :initarg :symbol :accessor room-symbol :documentation "ROOM标志")
   (description :initarg :description :accessor room-description :documentation "ROOM描述")
   (inventory :initarg :inventory :accessor room-inventory :documentation "ROOM中所有的物品")
   (enemy :initarg :enemy :accessor room-enemy :documentation "ROOM中所拥有的敌人")))

(defun build-room (text)
  "根据`text'创建room,并将room存入`rooms-alist'中"
  (cl-multiple-value-bind (symbol description) (split-string text "=")
	(setq symbol (intern symbol))
	(push (cons symbol (make-instance Room :symbol symbol :description description)) rooms-alist)))

(defun build-rooms(room-config-file)
  "根据`room-config-file'中的配置信息创建各个room"
  (let ((file-lines (remove-if (lambda (line)
								"是否以#开头的行"
								(string-match-p "^[[:blank:]]*#" line))
							   (split-string (file-content room-config-file) "[\r\n]"))))
	(mapc #'build-room file-lines)))


(defvar room-map nil
  "room的地图")

(defun build-room-map(room-map-config-file)
  "根据`room-map-config-file'中的配置信息创建地图"
  (let* ((file-lines (split-string (file-content room-map-config-file) "[\r\n]")))
	(setq room-map (mapcar (lambda(line)
							  (mapcar #'intern (split-string line)))
							file-lines))))

(defun get-room-position (room-symbol room-map)
  "从`room-map'中取出`room-symbol'标识的room的坐标"
  (let* ((x (cl-position-if (lambda(x-rooms)
							  (member room-symbol x-rooms)) room-map))
		 (y (cl-position room-symbol (nth x room-map))))
	(list x y)))

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

(provide 'room-maker)

(setq room0 (make-instance Room :description "miaos"))
