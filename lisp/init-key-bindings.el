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



;; define toggle/cycle command
(defun yd-move-indent ()
  (interactive)
  (if (get 'yd-move-indent 'state)
      (progn
        (call-interactively 'move-beginning-of-line)
        (put 'yd-move-indent 'state nil))
    (progn
      (call-interactively 'back-to-indentation)
      (put 'yd-move-indent 'state t))))

(defun yd-toggle-test ()
  (interactive)
  (if (not (get 'yd-toggle-test 'state))
      (put 'yd-toggle-test 'state "beginning"))

  (cond
   ((string= "beginning" (get 'yd-toggle-test 'state))
    (call-interactively 'move-beginning-of-line)
    (put 'yd-toggle-test 'state "indentation"))

   ((string= "indentation" (get 'yd-toggle-test 'state))
    (call-interactively 'back-to-indentation)
    (put 'yd-toggle-test 'state "end"))

   ((string= "end" (get 'yd-toggle-test 'state))
    (call-interactively 'move-end-of-line)
    (put 'yd-toggle-test 'state "beginning"))
   ))

;; Use minor mode for override key bindings
;; this is coming from here: https://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; C-x C-f was binded to counsel-find-file
    (define-key map (kbd "C-x C-f") 'projectile-find-file-in-known-projects)
    (define-key map (kbd "C-x b") 'helm-buffers-list)
    (define-key map (kbd "C-a") 'yd-move-indent)
    map)
  "my-keys-minor-mode keymap.")


(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)

(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

;; projectile-find-file-in-known-projects is very slow, so we need to enable the cache.
(setq projectile-enable-caching t)
;;; avy settings


(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
         (t (put this-command 'state "all lower") ) ) )
      )

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")) )
    )
  )

;;set this to M-c
(global-set-key "\M-c" 'toggle-letter-case)


;; symbol-overlay
(global-set-key (kbd "M-i") 'symbol-overlay-put)
(global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
(global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
(global-set-key (kbd "<f7>") 'symbol-overlay-mode)
(global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)

(provide 'init-key-bindings)
;;; init-key-bindings.el ends here
