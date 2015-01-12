(defun buffer-active-p(buffer)
  "判断buffer是否有在某个window中显示"
  (get-buffer-window buffer t))

(defun kill-buffer-if-not-active(buffer)
  "若buffer处于非active状态,则kill之"
  (unless (buffer-active-p buffer)
	(kill-buffer buffer)))

(defgroup clean-buffers nil
  "定时清理无用的buffer")

(defcustom kill-active-buffer nil
  "自动清理时是否清理active状态的buffer"
  :type '(boolean)
  :group 'clean-buffers)
(defcustom judge-useless-buffer-functions '(judge-useless-buffer-by-time judge-useless-buffer-by-name)
  "用于判断buffer是否为无用buffer的函数列表

用于判断buffer的函数会接收buffer作为唯一参数,并在buffer为无用时返回非nil"

  :group 'clean-buffers
  :type '(repeat function))

(defcustom useless-buffer-time-out (* 7 24 3600)
  "judge-useless-buffer-by-time中,超过该时长未显示的buffer被认为是无用的buffer,单位为秒"
  :group 'clean-buffers
  :type '(integer))

(defun judge-useless-buffer-by-time (buffer)
  "超过一定时间未显示的buffer被认为是无用的

超时时间由`useless-buffer-time-out'决定"
  (let (now buffer-last-display-time)
	(setq now (float-time (current-time)))
	(setq buffer-last-display-time (float-time (buffer-local-value 'buffer-display-time (get-buffer buffer))))
	(> (- now buffer-last-display-time) useless-buffer-time-out)))

(defcustom useless-buffer-names 
	'("*Buffer List*" "*Backtrace*" "*Apropos*" "*Completions*" "*Help*" "\\.~master~" "\\*vc-dir\\*")
	"无用buffer的名称列表"
	:group 'clean-buffers
	:type '(repeat regexp))

(defun judge-useless-buffer-by-name (buffer)
  ""
  (when (bufferp buffer)
	(setq buffer (buffer-name buffer)))
  (some (lambda (reg) (string-match reg buffer)) useless-buffer-names))

(defun useless-buffer-p (buffer)
  "使用judge-useless-buffer-functions中的函数判断buffer是否为无用的buffer"
  (let (useless-flag )
	(setq useless-flag 
		  (catch 'break
			(dolist (judge-useless-buffer-function judge-useless-buffer-functions)
			  (when (funcall judge-useless-buffer-function buffer)
				(throw 'break t)))
			nil))
	useless-flag))
(defun kill-useless-buffer(buffer)
  "若buffer为无用的buffer,则kill之"
  (when (useless-buffer-p buffer)
	(if kill-active-buffer
		(kill-buffer buffer)
	  (kill-buffer-if-not-active buffer))))

(defun kill-useless-buffers()
  "清理所有的无用buffer"
  (interactive)
  (dolist (buffer (buffer-list))
	(kill-useless-buffer buffer)))

(provide 'darksun-clean-buffers)
