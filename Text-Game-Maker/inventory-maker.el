(defvar display-fn #'message
  "显示信息的函数")
(defvar inventorys-alist nil
  "symbol与inventory对象的映射")

(defun get-inventory-by-symbol (symbol &optional noexception)
  "根据symbol获取inventory对象"
  (let (object)
	(setq object (cdr (assoc symbol inventorys-alist)))
	(when (and (null object)
			   (null noexception))
	  (throw 'exception (format "没有定义该物品[%s]" symbol)))
	object))

;; 定义Inventory类
(defclass Inventory nil
  ((symbol :initform (intern (format "inventory-%s" (length inventorys-alist))) :initarg :symbol :accessor member-symbol :documentation "INVENTORY标志")
   (description :initarg :description :accessor member-description :documentation "INVENTORY描述")
   (type :initarg :type :accessor member-type :documentation "INVENTORY的类型")
   (effects :initarg :effects :accessor member-effects :documentation "INVENTORY的使用效果")
   (watch-trigger :initform nil :initarg :watch-trigger :accessor member-watch-trigger :documentation "查看该INVENTORY时触发的事件")
   (get-trigger :initform nil :initarg :get-trigger :accessor member-get-trigger :documentation "获取该INVENTORY时触发的事件")
   (drop-trigger :initform nil :initarg :drop-trigger :accessor member-drop-trigger :documentation "丢弃该INVENTORY时触发的事件")
   (use-trigger :initform nil :initarg :use-trigger :accessor member-use-trigger :documentation "使用该INVENTORY时触发的事件")
   (wear-trigger :initform nil :initarg :wear-trigger :accessor member-wear-trigger :documentation "装备该INVENTORY时触发的事件")
   ))

(defmethod describe ((inventory Inventory))
  "输出inventory的描述"
	(format "这个是%s\n%s\n类型:%s\n使用效果:%s" (member-symbol inventory) (member-description inventory) (member-type inventory) (member-effects inventory)))

;; 创建inventory列表的方法
(defun build-inventory (inventory-entity)
  "根据`text'创建inventory"
  (cl-multiple-value-bind (symbol description type effects) inventory-entity
	(cons symbol (make-instance Inventory :symbol symbol :description description :type type :effects effects))))

(defun build-inventorys(inventory-config-file)
  "根据`inventory-config-file'中的配置信息创建各个inventory"
  (let ((inventory-entities (read-from-whole-string (file-content inventory-config-file))))
	(mapcar #'build-inventory inventory-entities)))

(defun inventorys-init(inventory-config-file)
  "初始化函数,生成inventory对象"
  (setq inventorys-alist (build-inventorys inventory-config-file)))

(defun inventory-has-type-p (inventory type)
  "`inventory'是否能被装备"
  (when (symbolp inventory)
	(setq inventory (get-inventory-by-symbol inventory)))
  (let ((inventory-type (member-type inventory)))
	(or (eq inventory-type type)
		(member type inventory-type))))

(defun inventory-usable-p (inventory)
  "`inventory'是否能被消耗"
  (inventory-has-type-p inventory 'usable))

(defun inventory-wearable-p (inventory)
  "`inventory'是否能被装备"
  (inventory-has-type-p inventory 'wearable))

(provide 'inventory-maker)
