;;; This File used to define my personal key bindings.
;;; First, I defined the leader key of <command> + SPACE.
;;; And every key that I defined will use this leader key.
;;; s


(progn
  ;; define a prefix keymap
  (define-prefix-command 'my-leader-key-map)
  ;; begin with s(search)
  ;;( (kbd "s ;") 'avy-goto-char)
  (define-key my-leader-key-map (kbd "s ;") 'avy-goto-char)
  (define-key my-leader-key-map (kbd "s d") 'dash-at-point)
  (define-key my-leader-key-map (kbd "s s") 'projectile-ag)
  ;; f(file)
  (define-key my-leader-key-map (kbd "f f") 'projectile-find-file)
  (define-key my-leader-key-map (kbd "f e") 'projectile-recentf)
  ;; begin with c(common)
  (define-key my-leader-key-map (kbd "c s") #'my-org-screenshot)
  )

(global-set-key (kbd "s-SPC") 'my-leader-key-map)
;;; avy settings

(provide 'init-key-bindings)
;;; init-key-bindings.el ends here
