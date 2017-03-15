;; -*- lexical-binding: t; -*-
(require 'w3m)
(require 'org-w3m)
(require 'eww)
(require 'org-eww)

(defgroup url2org nil
  "Save http(s) page as org file")

(defcustom url2org-store-dir "/home/lujun9972/github/emacs-document/raw/"
  "The directory to store org files"
  :type 'directory)

(defcustom url2org-auto-kill-p t
  "Whether kill the w3m/eww buffer after saved"
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

(defun url2org--copy-as-org ()
  (case major-mode
    ('w3m-mode
     (org-w3m-copy-for-org-mode))
    ('eww-mode
     (org-eww-copy-for-org-mode))
    (t (error "unsupported mode[%s]" major-mode))))

(defun url2org--get-current-url ()
  (case major-mode
    ('w3m-mode
     w3m-current-url)
    ('eww-mode
     (eww-current-url))
    (t (error "unsupported mode[%s]" major-mode))))

(defun url2org--get-current-title ()
  (case major-mode
    ('w3m-mode
     w3m-current-title)
    ('eww-mode
     (plist-get eww-data :title))
    (t (error "unsupported mode[%s]" major-mode))))

(defun url2org--save-to-org (&optional buf)
  (interactive)
  (let ((buf (or buf (current-buffer))))
    (with-current-buffer buf
      (message "storing %s" (url2org--get-current-title))
      (setq url2org-urls (delete (url2org--get-current-title) url2org-urls))
      (setq url2org-process-num (- url2org-process-num 1))
      (ignore-errors (url2org-save)))
    (when url2org-auto-kill-p
      (kill-buffer buf))))

(defun url2org-save ()
  (interactive)
  (let* ((url (url2org--get-current-url))
         (title (url2org--get-current-title))
         (filename (concat (file-name-as-directory url2org-store-dir) title ".org")))
    (url2org--copy-as-org)
    (with-temp-file filename
      (insert "#+URL: " url)
      (newline)
      (yank))))

;; (length url2org-urls)

(defun url2org (url)
  (interactive "surl:")
  (when url
    (pushnew url url2org-urls)
    (while (>= url2org-process-num url2org-max-process)
      (sit-for 1))
    (setq url2org-process-num (+ url2org-process-num 1))
    (case major-mode
      ('w3m-mode
       (w3m-goto-url-new-session url)
       (let ((buf (current-buffer)))
         (set (make-local-variable 'w3m-fontify-after-hook) (cons (lambda (&rest ignore)
                                                                    (url2org--save-to-org buf))
                                                                  w3m-fontify-after-hook))
         (run-with-timer url2org-timeout nil (lambda ()
                                               (when (buffer-live-p buf)
                                                 (setq url2org-process-num (- url2org-process-num 1))
                                                 (message "killing %s" url)
                                                 (kill-buffer buf))))))
      ('eww-mode
       (eww-browse-url url t)
       (let ((buf (current-buffer)))
         (set (make-local-variable 'eww-after-render-hook) (cons (lambda (&rest ignore)
                                                                   (url2org--save-to-org buf))
                                                                 eww-after-render-hook))
         (run-with-timer url2org-timeout nil (lambda ()
                                               (when (buffer-live-p buf)
                                                 (setq url2org-process-num (- url2org-process-num 1))
                                                 (message "killing %s" url)
                                                 (kill-buffer buf))))))
      (t "raise unsupported mode[%s]" major-mode))
    
    ))

;; (let ((url2org-store-dir "/home/lujun9972/github/lujun9972.github.com/英文必须死/"))
;;   (call-interactively #'url2org))
(provide 'url2org)
