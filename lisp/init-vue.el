(use-package lsp-vue :ensure)
(use-package lsp-mode :ensure)

(use-package company-quickhelp :ensure)
(use-package company-lsp
  :ensure
  :config
  ;; 开启 yasnippet 支持
  (setq company-lsp-enable-snippet t))

(use-package company
  :ensure
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.5)
  (setq company-idle-delay 0.5)
  (add-hook 'company-mode-hook 'company-quickhelp-mode)
  (add-to-list 'company-backends 'company-lsp))


(use-package web-mode
  :ensure
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook 'company-mode)
  (add-hook 'web-mode-hook 'lsp-vue-enable))
