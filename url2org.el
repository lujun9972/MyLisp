(require 'w3m)
(require 'f)
(defun url2org (url)
  (interactive "surl:")
  (w3m-goto-url url)
  (while w3m-current-process
    (sit-for 1))
  (let* ((title (w3m-current-title))
         (filename (format "/home/lujun9972/github/emacs-document/raw/%s.org" title))
         (content (progn
                    (org-w3m-copy-for-org-mode)
                    (with-temp-buffer
                      (run-hooks 'org-mode-hook)
                      (yank)
                      (buffer-string)))))
    (with-temp-file filename
      (insert content))))

;; (mapc (lambda (url)
;;         (ignore-errors
;;           (url2org url)))  '("https://blogs.msdn.microsoft.com/typescript/2015/11/03/what-about-asyncawait/" "https://utcc.utoronto.ca/~cks/space/blog/programming/CodeEditingVimVsEmacs"))
