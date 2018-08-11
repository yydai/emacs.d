;; https://johnsogg.github.io/emacs-golang
(setq tab-width 2)                      ; Four spaces is a tab

;; Define function to call when go-mode loads
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))

  ;; guru settings
  (go-guru-hl-identifier-mode)                    ; highlight identifiers

  ;; Key bindings specific to go-mode
  (local-set-key (kbd "M-.") 'godef-jump)         ; Go to definition
  (local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
  (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg

  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)
  ;; Misc go stuff
  )                         ; Enable auto-complete mode

;; Connect go-mode-hook with the function we just defined
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Ensure the go specific autocomplete is active in go-mode.
;; https://kknews.cc/tech/k2lkjq.html
;; https://github.com/OmniSharp/omnisharp-emacs/issues/137
;;

(with-eval-after-load 'go-mode
  (require 'golint)
  )


;; If the go-guru.el file is in the load path, this will load it.
(require 'go-guru)

(defun go-run ()
  (interactive)
  (setq filename (file-name-nondirectory buffer-file-name))
  (call-process-shell-command (format "go run %s" filename))
  )


(provide 'init-go)
