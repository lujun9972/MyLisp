;; 宣告背景程式的路徑
(defvar pry-program "pry"
  "Path to the program used by `run-pry'")

;; 宣告背景程式所需要的參數(arguments)
(defvar pry-arguments '()
  "Commandline arguments to pass to `pry'")

;; 設定提示符號格式，因為pry的比較簡單，就不需要複雜的regexp
(defvar pry-prompt-regexp "^\\[[0-9]+\\] pry([^()]+)[^>]*> "
  "Prompt for `run-pry'.")

(defvar pry-buffer "*Pry*"
  "buffer for `run-pry'")

;; 這坨代碼的目的是：重新啟動pry程式 (假如正在該*Pry*，且處於未活化狀態的話)  
;; 或者創造它(假如*Pry*)不存在的話。它的核心是apply那個語句。  
;; Run-pry  
(defun run-pry ()  
  "Run an inferior instance of `pry' inside Emacs."  
  (interactive)  
  (if (buffer-live-p (get-buffer pry-buffer)) 
	  (progn
		(pop-to-buffer-same-window (get-buffer pry-buffer)) ;若存在pry-buffer,则显示该buffer
		(when (or (not (comint-check-proc pry-buffer)) ;若pry-buffer对应进程被关闭
				  (with-current-buffer pry-buffer	  ;或pry-buffer不处于pry-mode
					(not (derived-mode-p 'pry-mode))))  
		  (error "something wrong with the %s buffer" pry-buffer))) ;则报错
	;; 若不存在pry-buffer,则新建之
	(let ((buffer (get-buffer-create  pry-buffer)))  
	  (apply 'make-comint-in-buffer pry-buffer buffer ;call pry
			 pry-program pry-arguments)  
	  (pry-mode))))  					;enable pry-mode

;; 關於 pry-mode (由 run-pry 呼叫)的細節設定  
(define-derived-mode pry-mode comint-mode "Pry"  
  "mode for `run-pry'"
  (setq comint-process-echoes t)  
  (setq comint-use-prompt-regexp t)      
  (setq comint-prompt-regexp pry-prompt-regexp)  
  (setq comint-prompt-read-only t)    ; 設定提示符號「> 」為只讀  
  ;; (set (make-local-variable 'paragraph-separate) "..'")  
  ;; 底下可以讓使用者以 M-{ 及 M-} 在各段落間移動  
  ;; (set (make-local-variable 'paragraph-start) pry-prompt-regexp)
  )


;; 將內容送往解釋器  
;; 送一個反白區域  
(defun pry-send-region (start end)  
  "Send the current region to the inferior Javascript process."  
  (interactive "r")  
  (comint-send-region (get-buffer-process pry-buffer) start end)  
  (comint-send-string (get-buffer-process pry-buffer) "\n"))  

;; 重新送出上次段落？  
(defun pry-send-last-sexp ()  
  "Send the previous sexp to the inferior Javascript process."  
  (interactive)  
  (pry-send-region (save-excursion (backward-sexp) (point)) (point)))  

;; 將整個 buffer 送出  
(defun pry-send-buffer ()  
  "Send the buffer to the inferior Javascript process."  
  (interactive)  
  (pry-send-region (point-min) (point-max)))  

(defvar pry-mode-map  
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))  
    (local-set-key (kbd "C-x C-e") 'pry-send-last-sexp)  
    (local-set-key (kbd "C-x C-b") 'pry-send-buffer)  
	map)  
  "Basic mode map for `run-pry'")  
