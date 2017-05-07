(require 'yasnippet)

;;; To use YASnippet as a non-global minor mode, don't call
;;; yas-global-mode; instead call yas-reload-all to load the snippet
;;; tables and then call yas-minor-mode from the hooks of major-modes
;;; where you want YASnippet enabled.

;;; see: https://github.com/joaotavora/yasnippet

;;; learn more about yasnippet, refer: http://joaotavora.github.io/yasnippet/snippet-organization.html

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        "~/.emacs.d/download-snippets/yasnippet-snippets"         ;; the default collection
        ))

(yas-global-mode 1)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
