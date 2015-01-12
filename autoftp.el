;;�Զ��ϴ��ļ�
;����ftp����ĵ�ַ
;(setq ange-ftp-ftp-programe-name "d:/bin/ftp_xp.exe")
(defvar *is-auto-ftp* nil
  "�Ƿ��Զ�ftp�ı�־")
(defun turn-on-auto-ftp()
  "���ô��Զ�ftp����"
  (interactive)
  (setf *is-auto-ftp* t))
(defun turn-off-auto-ftp()
  "���ù����Զ�ftp����"
  (interactive)
  (setf *is-auto-ftp* nil))

(defvar *remote-root* "/cnaps2@10.8.6.10:"
  "Զ��ftp�ĸ�Ŀ¼��ַ,��ʽΪ/username@ip:/path/to/root/")
(defun set-remote-root(remote-root)
  "����Զ��ftp�ĸ�Ŀ¼��ַ"
  (interactive "s������Զ��ftp�ĵ�ַ,��ʽΪ/username@ip:/path/to/root/")
  (setf *remote-root* (expand-file-name (file-name-as-directory remote-root))))

(defvar *local-root* (file-name-as-directory (expand-file-name "."))
  "*��ʶ�����ļ��Ŀ�ʼ��Ŀ¼,����Щ·����ͷ���ļ��Ż�ftp��Զ��")	
(defun set-local-root(local-root)
  "���ñ����ļ��Ŀ�ʼ��Ŀ¼"
  (interactive "s�����뱾���ļ��Ŀ�ʼ��Ŀ¼")
  (setf *local-root* (file-name-as-directory local-root)))

(defun local-path-to-remote-path(local-path)
  "ת������·��Ϊftp��Զ��·��"
  (replace-regexp-in-string (concat "^" (regexp-quote *local-root*)) *remote-root* local-path)) 

;; (local-path-to-remote-path "d:/workcvs/cnaps2/server/trunk/makeall")
(global-set-key [f11] 'UpMeToFtp)
(defun UpMeToFtp()
  "Upload me to the ftp "
  (interactive)
  (if (and *is-auto-ftp* (string-prefix-p *local-root* buffer-file-name))
	  (copy-file buffer-file-name (local-path-to-remote-path buffer-file-name) t)))  

(add-hook 'after-save-hook 'UpMeToFtp)

(provide 'autoftp)
