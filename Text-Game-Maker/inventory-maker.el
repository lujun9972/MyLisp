(defvar display-fn #'message
  "显示信息的函数")
(defvar inventorys-alist nil
  "symbol与inventory对象的映射")

(defun get-inventory-by-symbol (symbol)
  "根据symbol获取inventory对象"
  (cdr (assoc symbol inventorys-alist)))

;; 定义Inventory类
(defclass Inventory nil
  ((symbol :initform (intern (format "inventory-%s" (length inventorys-alist))) :initarg :symbol :accessor inventory-symbol :documentation "INVENTORY标志")
   (description :initarg :description :accessor inventory-description :documentation "INVENTORY描述")
   (type :initarg :type :accessor inventory-type :documentation "INVENTORY的类型")
   (effect :initarg :effect :accessor inventory-effect :documentation "INVENTORY的使用效果")
   (watch-trigger :initform nil :initarg :watch-trigger :accessor inventory-watch-trigger :documentation "查看该INVENTORY时触发的事件")
   (get-trigger :initform nil :initarg :get-trigger :accessor inventory-get-trigger :documentation "获取该INVENTORY时触发的事件")
   (use-trigger :initform nil :initarg :use-trigger :accessor inventory-use-trigger :documentation "使用该INVENTORY时触发的事件")
   (wear-trigger :initform nil :initarg :wear-trigger :accessor inventory-wear-trigger :documentation "装备该INVENTORY时触发的事件")
   ))

(defmethod describe ((inventory Inventory))
  "输出inventory的描述"
	(format "这个是%s\n%s\n类型:%s\n使用效果:%s" (inventory-symbol inventory) (inventory-description inventory) (inventory-type inventory) (inventory-effect inventory)))

;; 创建inventory列表的方法
(defun build-inventory (inventory-entity)
  "根据`text'创建inventory"
  (cl-multiple-value-bind (symbol description type effect) inventory-entity
	(cons symbol (make-instance Inventory :symbol symbol :description description :type type :effect effect))))

(defun build-inventorys(inventory-config-file)
  "根据`inventory-config-file'中的配置信息创建各个inventory"
  (let ((inventory-entities (read-from-whole-string (file-content inventory-config-file))))
	(mapcar #'build-inventory inventory-entities)))

(defun inventorys-init(inventory-config-file)
  "初始化函数,生成inventory对象"
  (setq inventorys-alist (build-inventorys inventory-config-file)))


(provide 'inventory-maker)
