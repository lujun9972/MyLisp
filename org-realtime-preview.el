(require 'org)
(require 'eww)
(defvar realtime-preview-output-file-name "realtime-preview-result.html"
  "预览产生的临时文件名称")

(defun realtime-preview-convert (output-file-name)
  "导出org为`output-file-name'中,并调用`eww-open-file'来查看"
  (let ((cb (current-buffer)))
    (save-excursion
      (org-export-to-file 'html output-file-name nil nil nil nil nil #'eww-open-file))
    (switch-to-buffer cb)))
    

(defun realtime-preview ()
  "导出org为`realtime-preview-output-file-name'中,并调用`eww-open-file'来查看"
  (interactive)
  (realtime-preview-convert realtime-preview-output-file-name))

(defun turn-on-realtime-preview ()
  "开启保存后自动预览"
  (interactive)
  (add-hook 'after-save-hook #'realtime-preview nil t))

(defun turn-off-realtime-preview ()
  "关闭保存后自动预览"
  (interactive)
  (remove-hook 'after-save-hook #'realtime-preview t))

(provide 'org-realtime-preview.el)
