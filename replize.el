;; 設定提示符號格式
(defvar replize-prompt-regexp "^\\[[0-9]+\\] replize([^()]+)[^>]*> "
  "Prompt for `run-replize'.")

(defvar-local replize-buffer "*Replize*"
  "buffer for `run-replize'")

;; 這坨代碼的目的是：重新啟動replize程式 (假如正在該*Replize*，且處於未活化狀態的話)  
;; 或者創造它(假如*Replize*)不存在的話。它的核心是apply那個語句。  
;; Run-replize  
(defun run-replize (replize-program replize-arguments)  
  "Run an inferior instance of `replize' inside Emacs."  
  (interactive (list (read-string "请输入程序名称: ")
                     (split-string-and-unquote (read-string "请输入程序参数: "))))  
  (setq replize-buffer (format "*%s*" replize-program))
  (if (buffer-live-p (get-buffer replize-buffer)) 
	  (progn
		(pop-to-buffer-same-window (get-buffer replize-buffer)) ;若存在replize-buffer,则显示该buffer
		(when (or (not (comint-check-proc replize-buffer)) ;若replize-buffer对应进程被关闭
				  (with-current-buffer replize-buffer	  ;或replize-buffer不处于replize-mode
					(not (derived-mode-p 'replize-mode))))  
		  (error "something wrong with the %s buffer" replize-buffer))) ;则报错
	;; 若不存在replize-buffer,则新建之
	(let ((buffer (get-buffer-create  replize-buffer)))  
	  (apply 'make-comint-in-buffer replize-buffer buffer ;call replize
			 replize-program replize-arguments)  
	  (pop-to-buffer-same-window buffer)
	  (replize-mode)))  					;enable replize-mode

;; 關於 replize-mode (由 run-replize 呼叫)的細節設定  
(define-derived-mode replize-mode comint-mode "Replize"  
  "mode for `run-replize'"
  (setq comint-process-echoes nil)  
  ;; (setq comint-use-prompt-regexp t)      
  ;; (setq comint-prompt-regexp replize-prompt-regexp)  
  ;; (setq comint-prompt-read-only t)    ; 設定提示符號「> 」為只讀  
  ;; (set (make-local-variable 'paragraph-separate) "..'")  
  ;; 底下可以讓使用者以 M-{ 及 M-} 在各段落間移動  
  ;; (set (make-local-variable 'paragraph-start) replize-prompt-regexp)
  )


;; 將內容送往解釋器  
;; 送一個反白區域  
(defun replize-send-region (start end)  
  "Send the current region to the inferior Javascript process."  
  (interactive "r")  
  (comint-send-region (get-buffer-process replize-buffer) start end)  
  (comint-send-string (get-buffer-process replize-buffer) "\n"))  

;; 重新送出上次段落？  
(defun replize-send-last-sexp ()  
  "Send the previous sexp to the inferior Javascript process."  
  (interactive)  
  (replize-send-region (save-excursion (backward-sexp) (point)) (point)))  

;; 將整個 buffer 送出  
(defun replize-send-buffer ()  
  "Send the buffer to the inferior Javascript process."  
  (interactive)  
  (replize-send-region (point-min) (point-max)))  

(defvar replize-mode-map  
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))  
    (local-set-key (kbd "C-x C-e") 'replize-send-last-sexp)  
    (local-set-key (kbd "C-x C-b") 'replize-send-buffer)  
	map)  
  "Basic mode map for `run-replize'")  

(provide 'replize)
