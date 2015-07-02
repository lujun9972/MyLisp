(add-to-list 'load-path (pwd))
(defvar display-fn #'message
  "显示信息的函数")
(require 'room-maker)
(map-init "room-description.el" "room-map.ini")
(require 'inventory-maker)
(inventorys-init "inventory-config.el")
(require 'creature-maker)
(creatures-init "creature-config.el")
(require 'action)
(watch '辣椒)
inventorys-alist
(watch)
(move right)
(get '辣椒)
(describe myself)

(add-inventory-to-creature myself '辣椒)
(use '辣椒)

(drop '辣椒)
