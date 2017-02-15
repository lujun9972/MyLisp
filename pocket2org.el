(require 'cl-lib)
(cl-defun pocket2org (&optional (offset 1) (count 10) (recur t))
  (let* ((response (pocket-api-get :offset offset :count count))
         (records (cdr (assoc-string "list" response)))
         (urls (mapcar (lambda (record)
                         (cdr (assoc-string 'resolved_url record)))
                       records)))
    (when (and urls recur)
      (mapcar #'url2org  urls)
      (pocket2org (+ offset count) count callback))))

(let ((url2org-store-dir "~/pocket")
      (url2org-max-process 2))
  (pocket2org 1500 1000))

;; http://paulgraham.com/avg.html
