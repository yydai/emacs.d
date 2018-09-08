;; workgroup mode
(require 'workgroups2)
(workgroups-mode 1)
(setq wg-prefix-key (kbd "C-c z"))
;; <prefix> <key>
;; <prefix> c          ; create workgroup
;; <prefix> A          ; rename workgroup
;; <prefix> k          ; kill workgroup
;; <prefix> v          ; switch to workgroup
;; <prefix> C-s        ; save session
;; <prefix> C-f        ; load session
;;Change workgroups session file
(setq wg-session-file "~/.emacs.d/.emacs_workgroups")

(provide 'init-workgroup)
