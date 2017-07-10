(fset 'indent-code
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 tab S-right] 0 "%d")) arg)))


(fset 'select-current-line
      [?\C-a ?\C-. ?\C-e])


(fset 'kill-current-line
      "\C-a\C-k")



(fset 'cut-current-line
      [?\C-a ?\C-. ?\C-e ?\M-w])


(fset 'copy-current-line
      [?\C-a ?\C-. ?\C-e ?\C-w])


(fset 'insert-sequence
      [f3 return])

(provide 'init-macro)
