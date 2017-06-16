(require 'dashboard)

(dashboard-setup-startup-hook)

(setq dashboard-items '(
                        (bookmarks . 15)
                        (projects . 5)))

(provide 'init-dashboard)
