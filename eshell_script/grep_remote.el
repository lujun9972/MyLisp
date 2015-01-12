(defvar eshell/grep_remote_hosts '("10.8.205.49" "10.8.6.9"))
(defun eshell/command-remote (host user password cmd)
  (concat "plink " user "@" host " -pw " password " \". ~/.profile;" cmd "\"")
)
;; (eshell/command-remote "10.8.6.10" "cnaps2" "123456" "ls")

(defun eshell/shell-command-remote (host user password cmd)
  (shell-command-to-string (eshell/command-remote host user password cmd)))
;; (eshell/shell-command-remote "10.8.6.10" "cnaps2" "123456" "ls")


;; (defun eshell/grep_remote(log_path matchRegxp)
;;   (let (cmd result)
;; 	(setq cmd (concat "cd " log_path ";" "grep " matchRegxp " *"))
;; 	(dolist (remote-host eshell/grep_remote_hosts)
;; 	  (message "%s" remote-host)
;; 	  (eshell/shell-command-remote remote-host "cnaps2" "123456" cmd))))
(defun eshell/grep_remote(log_path matchRegxp)
  (let (cmd result)
	(setq cmd (concat "cd " log_path ";" "grep " matchRegxp " *"))
	  (eshell/shell-command-remote remote-host "cnaps2" "123456" cmd)))
;; (eshell/grep_remote "~/log/2014/06/03" "123")





