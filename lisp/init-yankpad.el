(use-package yankpad
  :ensure t
  :defer 10
  :init
  (setq yankpad-file "~/yankpad.org")
  :config
  (bind-key "<f7>" 'yankpad-map)
  (bind-key "<f12>" 'yankpad-expand)
  ;; If you want to complete snippets using company-mode
  (add-to-list 'company-backends #'company-yankpad)
  ;; If you want to expand snippets with hippie-expand
  (add-to-list 'hippie-expand-try-functions-list #'yankpad-expand))
