;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(cl-defun pocket2org (&optional (offset 1) (count 10) (recur t))
  (let* ((response (pocket-api-get :offset offset :count count))
         (records (cdr (assoc-string "list" response)))
         (urls (mapcar (lambda (record)
                         (cdr (assoc-string 'resolved_url record)))
                       records)))
    (when urls
      (mapcar (lambda (url)
                (ignore-errors
                  (url2org url))) urls) )
    (when (and urls recur)
      (pocket2org (+ offset count) count callback))))

(let ((url2org-store-dir "/tmp")
      (url2org-max-process 3))
  (pocket2org 1800 1000 nil))

;; http://paulgraham.com/avg.html
