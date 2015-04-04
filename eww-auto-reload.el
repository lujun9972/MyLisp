(require 'url-file)
(require 'eww)
(defun eww-buffer-p (buffer-or-name)
  "判断buffer是否为eww buffer"
  (eq 'eww-mode (buffer-local-value 'major-mode (get-buffer buffer-or-name))))

(defun url-file-p (url)
  "判断url是否为file url"
  (url-type (url-generic-parse-url url)))

(defun url-file-modification-time (url)
  "FTP/FILE URL的文件修改时间"
  (nth 5 (url-file-file-attributes url)))

(defvar-local last-url-file-modification-time (current-time)
  "`eww-current-url'的最近更新时间")

(defun eww-reload-when-url-file-modified (&optional buffer)
  "若eww的url资源更新过,则重新加载

url必须是file url"
  (interactive)
  (unless buffer
	(setq buffer (current-buffer)))
  (unless (buffer-live-p buffer)
	(error "buffer has been killed"))
  (unless (eww-buffer-p buffer)
	(error "buffer is not in eww-mode"))
  (let ((cb (current-buffer)))
	(with-current-buffer buffer
	  (let ((eww-url-modification-time
			 (url-file-modification-time eww-current-url)))
		(when (time-less-p last-url-file-modification-time eww-url-modification-time)
		  (setq last-url-file-modification-time eww-url-modification-time)
		  (eww-reload))))
	(switch-to-buffer cb)))

(defvar eww-auto-reload-buffers nil)

(defun eww-auto-reload()
  "清理`eww-auto-reload-buffers',对其中的buffer,若对应的url-file更新了,则reload"
  (setq eww-auto-reload-buffers (remove-if-not #'buffer-live-p eww-auto-reload-buffers)) ;取出已经被killed的buffer
  (setq eww-auto-reload-buffers (remove-if-not #'eww-buffer-p eww-auto-reload-buffers))
  (mapc #'eww-reload-when-url-file-modified eww-auto-reload-buffers))

(defvar eww-auto-reload-timer nil
  "eww自动加载url的定时器")

(defvar eww-auto-reload-interval 1
  "eww auto reload的间隔时间")

(defun turn-on-eww-auto-reload()
  "开启eww自动reload"
  (interactive)
  (unless (and (eww-buffer-p (current-buffer))
			   (url-file-p eww-current-url))
	(error "该命令只能在eww-mode下执行,并且访问的url需要是file url"))
  (setq last-url-file-modification-time (current-time))
  (when (null eww-auto-reload-timer)
	(setq eww-auto-reload-timer (run-at-time 0 eww-auto-reload-interval #'eww-reload-when-url-file-modified)))
  (add-to-list 'eww-auto-reload-buffers (current-buffer)))

(defun turn-off-eww-auto-reload ()
  "关闭eww自动reload"
  (interactive)
  (setq eww-auto-reload-buffers (remove (current-buffer) eww-auto-reload-buffers))
  (when (null eww-auto-reload-buffers)
	(cancel-timer eww-auto-reload-timer)))

(provide 'eww-auto-reload)
