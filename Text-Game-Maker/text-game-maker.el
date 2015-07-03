(require 'tg-mode)
(defun tg-display (&rest args)
  (apply display-fn args))
(require 'room-maker)
(require 'inventory-maker)
(require 'creature-maker)
(require 'action)


(provide 'text-game-maker)
