(require 'dashboard)

(dashboard-setup-startup-hook)

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)))

(provide 'init-dashboard)
