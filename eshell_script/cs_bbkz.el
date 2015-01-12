(defun eshell-command-show-result(command &optional status-var)
  (message "%s" (eshell-command-result command status-var)))
(defun file-size(file-path)
  "获取文件的字节数"
  (nth 7 (file-attributes file-path)))
(defun file-concat(dir file)
  (concat (file-name-as-directory dir) file))
(defun file-md5(path)
  (with-temp-buffer
      (insert-file-contents path)
    (md5 (buffer-string))))

(defvar bbkz-operation-path
  "e:/cvsclient/Temp/bbkz"
  "版本控制的操作路径")
(defvar cvs-path
  "f:/cvsclient"
  "CVS路径")
(defvar bbkz-path
  "E:/FE_BBK"
  "版本库路径")

(defun cs-update-cvs(project)
  "更新cvs"
  (message "开始更新%s 的cvs" project)
  (cvs-update (file-concat cvs-pathproject) '("-A" "-d" "-C")))
(defun cs-backup-bbkz(project)
  "备份版本库"
  (let (bbkz-project bbkz-project-path)
	(setq bbkz-project (concat project "_bbkz"))
	(setq bbkz-project-path (file-concat bbkz-path bbkz-project))
	(when (not (file-exists-p bbkz-project-path))
	  (message "开始备份版本库[%s]" project)
	  (eshell/cd bbkz-path)			   
	  (eshell/mv project bbkz-project)
	  (eshell/cp "-R" bbkz-project project))))
(defun cs-run-FE()
  "运行校验源代码的程序"
  (eshell/cd  "E:/cvsclient/Temp/BBKZ_FE_SRC/")
   (eshell-command "E:/cvsclient/Temp/BBKZ_FE_SRC/BBKZ_FE.exe &"))
  
  ;; (eshell-do-subjob
  ;;  (eshell-command "E:/cvsclient/Temp/BBKZ_FE_SRC/BBKZ_FE.exe ")))

(defun cs-01-init-env(project)
  "初始化打包测试的环境
更新CVS版本
备份版本库
运行校验源代码的程序"
  (cs-update-cvs project)
  (cs-backup-bbkz project)
  (cs-run-FE))

(defun cs-fmt-srclist()
  "格式化源代码更新文件"
  (let (srclist-path prefix-string)
    (setq srclist-path (file-concat bbkz-operation-path "firstlist.txt"))
    (when (equal (buffer-file-name) srclist-path)
      (goto-line 1)
      (delete-matching-lines "^[ 	]*$")
      (setq prefix-string (file-name-as-directory (read-string "请输入源代码对应CVS目录")))
      (replace-regexp "^" (regexp-quote prefix-string) nil (point-min) (- (point-max) 1)) ;在每行头部补全路径
      (replace-regexp (regexp-quote "/") (regexp-quote "\\") nil (point-min) (point-max)) ;把/统一改为/
      (replace-regexp "^\\([^ 	]+\\)[ 	]+\\([0-9.]+\\)[ 	]*" "\\1\|\\2|" nil (point-min) (point-max)) ;格式化为path|version格式
    (set-buffer-file-coding-system 'utf-8-dos) ;转换为DOS格式
      )))

(defun cs-02-gen-srctlist()
  "生成源代码更新清单文件"
  (let (srclist-path)
    (setq srclist-path (file-concat bbkz-operation-path "firstlist.txt"))
    (find-file-other-window srclist-path)
    (make-local-variable 'beofore-save-hook)
    (add-to-list 'before-save-hook 'cs-fmt-srclist)))

(defun cs-03-copy-src-to-bbkz-windows(project)
  (let (src-path)
    (setq src-path (file-concat bbkz-operation-path project))
    (eshell/cp "-Rp" src-path bbkz-path)))

(defun cs-04-chksrc-aix()
  (find-file-other-window (file-concat bbkz-operation-path "checksrc.txt")))
(defun cs-04-chksrc-windows()
  (eshell/cd bbkz-operation-path)
  (eshell-command-show-result "cmd /C bbkz-04-WChksrc.bat")
  (find-file-other-window "chksrc.txt"))

(defun cs-05-gen-objlist-aix()
  "生成obj.txt"
  (let (objlist-path)
    (setq objlist-path (file-concat bbkz-operation-path "obj.txt"))
    (make-local-variable 'beofore-save-hook)
    (find-file-other-window objlist-path)
    (add-to-list 'before-save-hook 'cs-fmt-objlist-aix)))
(defun cs-fmt-objlist-aix()
  "格式化obj.txt"
  (let (objlist-path)
    (setq objlist-path (file-concat bbkz-operation-path "obj.txt"))
    (when (equal (buffer-file-name) objlist-path)
      (replace-regexp "^\\([^ 	]+\\)[ 	]*$" "\\1" nil (point-min) (point-max)) ;去掉结尾的空格
      (set-buffer-file-coding-system 'utf-8-unix) ;转换为UNIX格式
      )))



(defun cs-05-gen-objlist-windows()
  "生成objlist.txt"
  (let (objlist-path)
    (setq objlist-path (file-concat bbkz-operation-path "objlist.txt"))
    (find-file-other-window objlist-path)
    (make-local-variable 'beofore-save-hook)
    (add-to-list 'before-save-hook 'cs-fmt-objlist-windows)))
(defun cs-fmt-objlist-windows()
  "格式化obj.txt"
  (let (objlist-path prefix-string)
    (setq objlist-path (file-concat bbkz-operation-path "objlist.txt"))
    (when (equal (buffer-file-name) objlist-path)
      (setq prefix-string (file-name-as-directory (read-string "请输入源代码对应CVS目录")))
      (replace-regexp "^" (regexp-quote prefix-string) nil (point-min) (- (point-max) 1)) ;在每行头部补全路径
      (replace-regexp (regexp-quote "/") (regexp-quote "\\") nil (point-min) (point-max)) ;把/统一改为/
      (replace-regexp "^\\([^ 	]+\\)[ 	]*$" "\\1" nil (point-min) (point-max)) ;去掉结尾的空格
      (set-buffer-file-coding-system 'utf-8-dos) ;转换为DOS格式
      )))

(defvar cs-clean-path
  "Z:/DRCB_测试环境/cnaps2/"
  "测试环境打包目录")
(defun cs-clean(project)
  "清理测试环境"
  (let (today seq newDir)
    (setq today (format-time-string "%Y%m%d"))
    (setq seq (string-to-int (eshell-command-result (concat "ls -d " cs-clean-path today "* 2>/dev/null |wc -l"))))
    (cl-incf seq)
    (setq newDir (concat cs-clean-path today (format "%02d" seq)))
    (message "新建目录[%s]" newDir)
    (eshell/mkdir newDir)
    (eshell/cd bbkz-operation-path)
    (eshell-command (format "mv ${C:/Program\\ Files/bin/UnxUtils/usr/local/wbin/find.exe -newer firstlist.txt -type f -o -name firstlist.txt} %s" newDir))
    (eshell-command (format "explorer %s" (convert-standard-filename newDir)))))
