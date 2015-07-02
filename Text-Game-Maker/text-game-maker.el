(require 'room-maker)
(require 'inventory-maker)
(require 'creature-maker)
(require 'action)
(require 'tg-mode)

(defun tg-display (&rest args)
  (apply display-fn args))

(provide 'text-game-maker)
