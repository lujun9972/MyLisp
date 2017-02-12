;; -*- lexical-binding: t; -*-
(require 'w3m)

(defgroup url2org nil
  "save http(s) page to org file")

(defcustom url2org-store-dir "/home/lujun9972/github/emacs-document/raw/"
  "specify the directory to store org files"
  :type 'directory)

(defcustom url2org-auto-kill-p t
  "wether kill the w3m buffer after saved to org file"
  :type 'boolean)

(defun url2org--save-to-org (&rest ignore)
  (message "storing %s" w3m-current-url)
  (let* ((title (w3m-current-title))
         (filename (concat (file-name-as-directory url2org-store-dir) title ".org"))
         (content (progn
                    (org-w3m-copy-for-org-mode)
                    (with-temp-buffer
                      (run-hooks 'org-mode-hook)
                      (yank)
                      (goto-char (point-min))
                      (search-forward "location")
                      (replace-match "#+URL")
                      (buffer-string)))))
    (with-temp-file filename
      (insert content))
    (when url2org-auto-kill-p
      (kill-buffer))))

(defun url2org (url)
  (interactive "surl:")
  (w3m-goto-url-new-session url)
  (set (make-local-variable 'w3m-display-hook) (cons #'url2org--save-to-org w3m-display-hook)))

;; (mapc (lambda (url)
;;         (ignore-errors
;;           (url2org url)))  '("https://blogs.msdn.microsoft.com/typescript/2015/11/03/what-about-asyncawait/" "https://utcc.utoronto.ca/~cks/space/blog/programming/CodeEditingVimVsEmacs"))

;; (mapc 'url2org '("http://www.baidu.com" "http://www.bing.com"))
