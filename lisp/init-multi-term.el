(setq multi-term-program "/bin/zsh")

(custom-set-variables
 '(term-default-bg-color "#000000")        ;; background color (black)
 '(term-default-fg-color "#dddd00"))       ;; foreground color (yellow)

(add-hook 'term-mode-hook
          (lambda ()
            (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))))

(add-hook 'term-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)
            (autopair-mode -1)))

(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y") 'term-paste)))

(provide 'init-multi-term)
