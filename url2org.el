;; -*- lexical-binding: t; -*-
(require 'w3m)

(defgroup url2org nil
  "Save http(s) page as org file")

(defcustom url2org-store-dir "/home/lujun9972/github/emacs-document/raw/"
  "The directory to store org files"
  :type 'directory)

(defcustom url2org-auto-kill-p t
  "Whether kill the w3m buffer after saved"
  :type 'boolean)

(defcustom url2org-max-process 10
  "Max number of w3m process"
  :type 'number)

(defvar url2org-process-num 0
  "Number of w3m process")

(defvar url2org-urls nil)

(defcustom url2org-timeout 30
  "Timeout seconds"
  :type 'number)

(defun url2org--save-to-org (w3m-buf)
  (with-current-buffer w3m-buf
    (message "storing %s" w3m-current-url)
    (setq url2org-urls (delete w3m-current-title url2org-urls))
    (setq url2org-process-num (max 0 (- url2org-process-num 1)))
    (url2org-save)
    (when url2org-auto-kill-p
      (kill-buffer))))

(defun url2org-save ()
  (interactive)
  (let* ((title (w3m-current-title))
         (filename (concat (file-name-as-directory url2org-store-dir) title ".org"))
         (content (progn
                    (org-w3m-copy-for-org-mode)
                    (with-temp-buffer
                      (yank)
                      (buffer-string)))))
    (with-temp-file filename
      (insert "#+URL: " w3m-current-url)
      (newline)
      (insert content))))

(defun url2org (url)
  (interactive "surl:")
  (push url url2org-urls)
  (while (>= url2org-process-num url2org-max-process)
    (sit-for 1))
  (setq url2org-process-num (+ url2org-process-num 1))
  (w3m-goto-url-new-session url)
  (let ((buf (current-buffer)))
    (run-with-timer url2org-timeout nil (lambda ()
                                          (when (buffer-live-p buf)
                                            (setq url2org-process-num (- url2org-process-num 1))
                                            (kill-buffer buf))))
    (set (make-local-variable 'w3m-display-hook) (lambda (&rest ignore)
                                                   (url2org--save-to-org buf)))))
