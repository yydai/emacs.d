;;
;; (load "preview-latex.el" nil t t)
(require-package 'auctex)
;; (require 'auctex-autoloads)
(mapc (lambda (mode)
        (add-hook 'LaTeX-mode-hook mode))
      (list ;;
       'auto-fill-mode
       'flyspell-mode
       'LaTeX-math-mode
       'turn-on-reftex
       'TeX-fold-mode
       'linum-mode
       'auto-complete-mode
       'autopair-mode
       'outline-minor-mode))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq ;;TeX-auto-untabify t     ; remove all tabs before saving
             TeX-engine 'xetex       ; use xelatex default
             TeX-show-compilation nil) ; display compilation windows
            (TeX-global-PDF-mode t)       ; PDF mode enable, not plain
            (setq TeX-save-query nil)
            (imenu-add-menubar-index)
            ;;(define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)
            ))

;; configuration for TeX-fold-mode
;; add entries you want to be fold, or comment that needn't to be fold.
(setq TeX-fold-env-spec-list
      (quote (("[figure]" ("figure"))
              ("[table]" ("table"))
              ("[itemize]" ("itemize"))
              ("[description]" ("description"))
              ("[tabular]" ("tabular"))
              ("[frame]" ("frame"))
              ("[array]" ("array"))
              ("[code]" ("lstlisting"))
              ;;              ("[eqnarray]" ("eqnarray"))
              )))

;; configuration for reftex

;; make the toc displayed on the left
(setq reftex-toc-split-windows-horizontally t)
;; adjust the fraction
(setq reftex-toc-split-windows-fraction 0.3)
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq TeX-output-view-style
      (quote
       (("^pdf$" "." "evince -f %o")
        ("^html?$" "." "iceweasel %o"))))


;;; copy from other place
;; AucTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
                             (push
                              '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
                                :help "Run latexmk on file")
                              TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list`
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))


(provide 'init-auctex)
