(defvar rooms-alist nil
  "symbol与room对象的映射")

;; 创建ROOM
(defclass Room nil
  ((symbol :initform (intern (format "room-%s" (length rooms-alist))) :initarg :symbol :accessor room-symbol :documentation "ROOM标志")
   (description :initarg :description :accessor room-description :documentation "ROOM描述")
   (inventory :initarg :inventory :accessor room-inventory :documentation "ROOM中所有的物品")
   (enemy :initarg :enemy :accessor room-enemy :documentation "ROOM中所拥有的敌人")))

(defun build-room (text)
  "根据`text'创建room"
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

(defun build-room-map(room-map-config-file)
  "根据`room-map-config-file'中的配置信息创建地图"
  )

(provide 'room-maker)

(setq room0 (make-instance Room :description "miaos"))
