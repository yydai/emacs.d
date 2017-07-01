(require 'cc-mode)
(require 'semantic)

;; company-c-headers
(use-package company-c-headers
             :init
             (add-to-list 'company-backends 'company-c-headers))

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
;; (setq c-default-style) "linux" ;; set style to "linux"

(use-package cc-mode
  :init
  (define-key c-mode-map  [(tab)] 'company-complete)
  (define-key c++-mode-map  [(tab)] 'company-complete))


;; --------------
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-stickyfunc-mode 1)

(semantic-mode 1)

(defun alexott/cedet-hook ()
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'c-mode-hook 'alexott/cedet-hook)
(add-hook 'c++-mode-hook 'alexott/cedet-hook)

;; Enable EDE only in C/C++
(require 'ede)
(global-ede-mode)



;; ----------------
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; company
(use-package company
             :init
             (global-company-mode 1)
             (delete 'company-semantic company-backends))
;; (define-key c-mode-map  [(control tab)] 'company-complete)
;; (define-key c++-mode-map  [(control tab)] 'company-complete)

;; Package: projejctile
(use-package projectile
             :init
             (projectile-global-mode)
             (setq projectile-enable-caching t))

;; Package zygospore
(use-package zygospore
             :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
                    ("RET" .   newline-and-indent)))

                                        ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)
(windmove-default-keybindings)


;; ------------------
(use-package ivy
  :init
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key (kbd "C-c s") 'swiper)))

(use-package counsel
             :bind
             (("M-x" . counsel-M-x)
              ("M-y" . counsel-yank-pop)
              ("C-c r" . counsel-recentf)
              ("C-x C-f" . counsel-find-file)
              ("<f1> f" . counsel-describe-function)
              ("<f1> v" . counsel-describe-variable)
              ("<f1> l" . counsel-load-library)
              ("C-h f" . counsel-describe-function)
              ("C-h v" . counsel-describe-variable)
              ("C-h l" . counsel-load-library)))

(use-package counsel-projectile
             :init
             (counsel-projectile-on))


;;

(use-package helm
             :init
             (progn
               (require 'helm-config)
               (require 'helm-grep)
               ;; To fix error at compile:
               ;; Error (bytecomp): Forgot to expand macro with-helm-buffer in
               ;; (with-helm-buffer helm-echo-input-in-header-line)
               (if (version< "26.0.50" emacs-version)
                   (eval-when-compile (require 'helm-lib)))

               (defun helm-hide-minibuffer-maybe ()
                 (when (with-helm-buffer helm-echo-input-in-header-line)
                   (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
                     (overlay-put ov 'window (selected-window))
                     (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                             `(:background ,bg-color :foreground ,bg-color)))
                     (setq-local cursor-type nil))))

               (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
               ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
               ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
               ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
               (global-set-key (kbd "C-c h") 'helm-command-prefix)
               (global-unset-key (kbd "C-x c"))

               (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
               (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
               (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

               (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
               (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
               (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

               (when (executable-find "curl")
                 (setq helm-google-suggest-use-curl-p t))

               (setq helm-google-suggest-use-curl-p t
                     helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
                     ;; helm-quick-update t ; do not display invisible candidates
                     helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

                     ;; you can customize helm-do-grep to execute ack-grep
                     ;; helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
                     ;; helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
                     helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window

                     helm-echo-input-in-header-line t

                     ;; helm-candidate-number-limit 500 ; limit the number of displayed canidates
                     helm-ff-file-name-history-use-recentf t
                     helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
                     helm-buffer-skip-remote-checking t

                     helm-mode-fuzzy-match t

                     helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non-nil
                                        ; useful in helm-mini that lists buffers
                     helm-org-headings-fontify t
                     ;; helm-find-files-sort-directories t
                     ;; ido-use-virtual-buffers t
                     helm-semantic-fuzzy-match t
                     helm-M-x-fuzzy-match t
                     helm-imenu-fuzzy-match t
                     helm-lisp-fuzzy-completion t
                     ;; helm-apropos-fuzzy-match t
                     helm-buffer-skip-remote-checking t
                     helm-locate-fuzzy-match t
                     helm-display-header-line nil)

               (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

               (global-set-key (kbd "M-x") 'helm-M-x)
               (global-set-key (kbd "M-y") 'helm-show-kill-ring)
               (global-set-key (kbd "C-x b") 'helm-buffers-list)
               (global-set-key (kbd "C-x C-f") 'helm-find-files)
               (global-set-key (kbd "C-c r") 'helm-recentf)
               (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
               (global-set-key (kbd "C-c h o") 'helm-occur)
               (global-set-key (kbd "C-c h o") 'helm-occur)

               (global-set-key (kbd "C-c h w") 'helm-wikipedia-suggest)
               (global-set-key (kbd "C-c h g") 'helm-google-suggest)

               (global-set-key (kbd "C-c h x") 'helm-register)
               ;; (global-set-key (kbd "C-x r j") 'jump-to-register)

               (define-key 'help-command (kbd "C-f") 'helm-apropos)
               (define-key 'help-command (kbd "r") 'helm-info-emacs)
               (define-key 'help-command (kbd "C-l") 'helm-locate-library)

               ;; use helm to list eshell history
               (add-hook 'eshell-mode-hook
                         #'(lambda ()
                             (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

;;; Save current position to mark ring
               (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

               ;; show minibuffer history with Helm
               (define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
               (define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

               (define-key global-map [remap find-tag] 'helm-etags-select)

               (define-key global-map [remap list-buffers] 'helm-buffers-list)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
               ;; PACKAGE: helm-swoop                ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
               ;; Locate the helm-swoop folder to your path
               (use-package helm-swoop
                            :bind (("C-c h o" . helm-swoop)
                                   ("C-c s" . helm-multi-swoop-all))
                            :config
                            ;; When doing isearch, hand the word over to helm-swoop
                            (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

                            ;; From helm-swoop to helm-multi-swoop-all
                            (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

                            ;; Save buffer when helm-multi-swoop-edit complete
                            (setq helm-multi-swoop-edit-save t)

                            ;; If this value is t, split window inside the current window
                            (setq helm-swoop-split-with-multiple-windows t)

                            ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
                            (setq helm-swoop-split-direction 'split-window-vertically)

                            ;; If nil, you can slightly boost invoke speed in exchange for text color
                            (setq helm-swoop-speed-or-color t))

               (helm-mode 1)

               (use-package helm-projectile
                            :init
                            (helm-projectile-on)
                            (setq projectile-completion-system 'helm)
                            (setq projectile-indexing-method 'alien))))



;; -------

;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

(use-package helm-gtags
  :init
  (progn
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-prefix-key "\C-cg"
          helm-gtags-suggested-key-mapping t)

    ;; Enable helm-gtags-mode in Dired so you can jump to any tag
    ;; when navigate project tree with Dired
    (add-hook 'dired-mode-hook 'helm-gtags-mode)

    ;; Enable helm-gtags-mode in Eshell for the same reason as above
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)

    ;; Enable helm-gtags-mode in languages that GNU Global supports
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'java-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)

    ;; key bindings
    (with-eval-after-load 'helm-gtags
      (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
      (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
      (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
      (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
      (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
      (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))))
