(require 'cl-lib)
(cl-defun pocket2org (&optional (offset 1) (count 10) (callback #'message))
  (let* ((response (pocket-api-get :offset offset :count count))
         (records (cdr (assoc-string "list" response)))
         (urls (mapcar (lambda (record)
                         (cdr (assoc-string 'resolved_url record)))
                       records)))
    (when urls
      (mapcar callback  urls)
      (pocket2org (+ offset count) count callback))))

(let ((url2org-store-dir "/tmp")
      (url2org-max-process 1))
  (pocket2org 1 1000 (lambda (url)
                       (ignore-errors (url2org url)))))
;; http://paulgraham.com/avg.html
