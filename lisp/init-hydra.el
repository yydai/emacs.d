;; window manage
(defhydra hydra-zoom-window (global-map "M-o")
  "
zoom window
^^^^^^^^-----------------------------------------------------------------
"
  ("j" window-enlarge "window enlarge")
  ("k" window-shrunk "window shrunk")
  ("o" yd-toggle-window-state "toggle window state"))


;; wg-create-workgroup
;; M-u is binded to upcase-word
(defhydra hydra-workgroups (global-map "M-u")
  "
workgroup
^^^^^^^^-----------------------------------------------------------------
[c] create
[r] rename
[s] switch workgroup
[p] previous workgroup
[n] next workgroup
[1] first group
[2] second group
[3] third group
"
("c" wg-create-workgroup "create work group")
("1" wg-switch-to-workgroup-at-index-0 "w0")
("2" wg-switch-to-workgroup-at-index-1 "w1")
("3" wg-switch-to-workgroup-at-index-2 "w2")
("4" wg-switch-to-workgroup-at-index-3 "w3")
("5" wg-switch-to-workgroup-at-index-4 "w4")
("6" wg-switch-to-workgroup-at-index-5 "w5")
("7" wg-switch-to-workgroup-at-index-6 "w6")
("8" wg-switch-to-workgroup-at-index-7 "w7")
("r" wg-rename-workgroup "rename")
("s" wg-switch-to-workgroup "switch group")
("p" wg-switch-to-workgroup-left "previous group")
("n" wg-switch-to-workgroup-right "next group")
)

(provide 'init-hydra)
