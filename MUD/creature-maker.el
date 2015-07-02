(defvar display-fn #'message
  "显示信息的函数")
(defvar creatures-alist nil
  "symbol与creature对象的映射")

(defun get-creature-by-symbol (symbol)
  "根据symbol获取creature对象"
  (cdr (assoc symbol creatures-alist)))

;; 定义Creature类
(defclass Creature nil
  ((symbol :initform (intern (format "creature-%s" (length creatures-alist))) :initarg :symbol :accessor creature-symbol :documentation "CREATURE标志")
   (description :initarg :description :accessor creature-description :documentation "CREATURE描述")
   (occupation :initform 'human :initarg :occupation :accessor creature-occupation :documentation "CREATURE的职业")
   (attr :initform nil :initarg :attr :accessor creature-attr :documentation "CREATURE的属性")
   (inventory :initform nil :initarg :inventory :accessor creature-inventory :documentation "CREATURE所只有的物品")
   (equipment :initform nil :initarg :equipment :accessor creature-equipment :documentation "CREATURE装备的装备")
   (watch-trigger :initform nil :initarg :watch-trigger :accessor creature-watch-trigger :documentation "查看该CREATURE后触发的事件")
   ))

(defmethod describe ((creature Creature))
  "输出creature的描述"
	(format "这个是%s\n%s\n属性值:%s\n拥有物品:%s\n装备了:%s" (creature-symbol creature) (creature-description creature) (creature-attr creature) (creature-inventory creature) (creature-equipment creature)))

;; 创建creature列表的方法
(defun build-creature (creature-entity)
  "根据`text'创建creature,并将creature存入`creatures-alist'中"
  (cl-multiple-value-bind (symbol description attr inventory equipment ) creature-entity
	(cons symbol (make-instance Creature :symbol symbol :description description :inventory inventory :equipment equipment :attr attr))))

(defun build-creatures(creature-config-file)
  "根据`creature-config-file'中的配置信息创建各个creature"
  (let ((creature-entities (read-from-whole-string (file-content creature-config-file))))
	(mapcar #'build-creature creature-entities)))

;; 定义初始化函数
(defvar myself nil				;
  "当前所处的creature对象")

(defun creatures-init(creature-config-file)
  "初始化函数,生成creature对象,组装map"
  (setq creatures-alist (build-creatures creature-config-file))
  (setq myself (get-creature-by-symbol (caar creatures-alist))))


(defun remove-inventory-from-creature (creature inventory)
  ""
  (setf (creature-inventory creature) (remove inventory (creature-inventory creature))))

(defun add-inventory-to-creature (creature inventory)
  ""
  (push inventory (creature-inventory creature)))

(defun inventory-exist-in-creature-p (creature inventory)
  ""
  (member inventory (creature-inventory creature)))

(defun remove-equipment-from-creature (creature equipment)
  ""
  (setf (creature-equipment equipment) (remove equipment (creature-equipment creature))))

(defun add-equipment-to-creature (creature equipment)
  ""
  (push equipment (creature-equipment creature)))

(defun equipment-exist-in-creature-p (creature equipment)
  ""
  (member equipment (creature-equipment creature)))

(defun take-effect-to-creature (creature effect)
  ""
  (let* ((attr-type (car effect))
		(value (cdr effect)))
	(if (assoc attr-type (creature-attr creature))
		(incf (cdr (assoc attr-type (creature-attr creature))) value)
		;; (setf (cdr (assoc attr-type (creature-attr creature))) (+ value old-value))
	  (push (copy-list effect ) (creature-attr creature)))))

(defun take-effects-to-creature(creature effects)
  ""
  (dolist (effect effects)
	(take-effect-to-creature creature effect)))

(provide 'creature-maker)

