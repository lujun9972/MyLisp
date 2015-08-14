;; 以下是server端代码
(require 'elnode)
(defvar webchat-content ""
  "聊天内容")
(defun webchat-server--get-content-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-Type" . "text/plain"))
  (elnode-http-return httpcon webchat-content))

(defun webchat-server--say-handler (httpcon)
  (let ((who (elnode-http-param httpcon "who"))
		(content (elnode-http-param httpcon "content")))
	  (if (stringp content)
		  (setq webchat-content (concat webchat-content (format "%s:\t%s\n" who content)))))
  (elnode-http-start httpcon 302 '("Location" . "/"))
  (elnode-http-return httpcon))

(defconst webchat-urls
  `(("^/$" . webchat-server--get-content-handler)
	("^/update/.*$" . webchat-server--say-handler)))


(defun webchat-server--dispatcher-handler (httpcon)
  (elnode-dispatcher httpcon webchat-urls))

(defun webchat-server()
  (interactive)
  (let ((port (read-number "请输入监听端口: ")))
	(elnode-start 'webchat-server--dispatcher-handler :port port)))

;; 以下是client端代码
(require 'url)
(defvar webchat-client-service-host "localhost"
  "webchat的服务器地址")
(defvar webchat-client-service-port 8000
  "webchat的服务器监听端口")
(defun webchat-client--get-content(&optional host port)
  (setq host (or host webchat-client-service-host))
  (setq port (or port webchat-client-service-port))
  (let ((buf (url-retrieve-synchronously (format "http://%s:%s/" host port)))
		content)
	(with-current-buffer buf
	  (goto-char (point-min))
	  (search-forward-regexp "^$")
	  (setq content (buffer-substring-no-properties (+ (point )1) (point-max))))
	(kill-buffer buf)
	content))

(defun webchat-client--say(who content &optional host port)
  (setq host (or host webchat-client-service-host))
  (setq port (or port webchat-client-service-port))
  (let ((buf (url-retrieve-synchronously
			   (format "http://%s:%s/update/?who=%s&content=%s"
					   host
					   port
					   (url-hexify-string who)
					   (url-hexify-string content))))
		 content)
	 (with-current-buffer buf
	   (goto-char (point-min))
	   (search-forward-regexp "^$")
	   (setq content (buffer-substring-no-properties (+ (point )1) (point-max))))
	(kill-buffer buf)
	content))
(defvar webchat-client-buffer "*webchat*"
  "显示聊天内容的buffer")
(defun webchat-client--display-content()
  "在buffer内显示聊天内容"
  (let ((content (webchat-client--get-content))
		(cb (current-buffer)))
	(save-excursion 
	  (select-or-create-buffer-window (get-buffer-create webchat-client-buffer))
	  ;; (goto-char (point-max))
	  ;; (insert (substring content (point)))
	  (erase-buffer)
	  (insert (decode-coding-string content 'utf-8-dos))
	  )
	(select-or-create-buffer-window cb)))

;; (defun webchat-talk()
;;   (interactive)
;;   (let* ((who (read-string "请输入你的名称: "))
;; 		 (content (format "%s进入了聊天室" who))
;; 		 (get-content-timer (run-with-idle-timer 1 t #'webchat-client--display-content)))
;; 	(unless (string= content "")
;; 	  (webchat-client--say who content)
;; 	  (setq content (read-string ": ")))
;; 	(cancel-timer get-content-timer)))

(define-derived-mode webchat-mode text-mode "WebChat"
  "Major mode for running webchat"
  (make-local-variable 'scroll-step)
  (setq scroll-step 2)
  (local-set-key (kbd "<RET>") #'webchat-client--talk))

(defvar webchat-client-who user-login-name
  "webchat客户名")
(defun webchat-client--talk ()
  (interactive)
  "Function called when return is pressed in interactive mode to talk"
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))
    (webchat-client--say webchat-client-who content)
	(erase-buffer)))

(defvar webchat-client--timer nil)
(defun webchat-talk ()
  (interactive)
  (setq webchat-client-who (read-string "请输入你的名称: " webchat-client-who))
  ;; (setq webchat-client--timer (run-with-idle-timer 1 1 #'webchat-client--display-content))
  (setq webchat-client--timer (run-with-timer 1 1 #'webchat-client--display-content))
  (webchat-mode 1))

(defun webchat-quit ()
  (interactive)
  (cancel-timer webchat-client--timer)
  (select-window (get-buffer-window webchat-client-buffer))
  (kill-buffer-and-window))
