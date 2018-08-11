(require 'dashboard)

(dashboard-setup-startup-hook)

(setq dashboard-items '(
                        (bookmarks . 15)
                        (projects . 5)))

(setq dashboard-startup-banner "~/.emacs.d/img/logo.png")
(setq dashboard-banner-logo-title "Nothing Special")

(provide 'init-dashboard)
