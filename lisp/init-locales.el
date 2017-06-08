;; packages
;; elpy, py-



(defun sanityinc/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun sanityinc/locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (sanityinc/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (sanityinc/utf8-locale-p (getenv "LC_ALL"))
      (sanityinc/utf8-locale-p (getenv "LC_CTYPE"))
      (sanityinc/utf8-locale-p (getenv "LANG"))))

(when (or window-system (sanityinc/locale-is-utf8-p))
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8))



;;; instead use erase-buffer use 'clear-buffer'
(defun clear-buffer ()
  (interactive)
  (erase-buffer))

;;;g

(defun ydai-run-current-file ()
  "Execute the current file.
For example, if the current buffer is x.py, then it'll call 「python x.py」 in a shell. Output is printed to message buffer.
The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, TypeScript, Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
File suffix is used to determine what program to run.

If the file is modified or not saved, save it automatically before run.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
Version 2017-02-10"
  (interactive)
  (let (
        (-suffix-map
         ;; (‹extension› . ‹shell program name›)
         `(
           ("php" . "php")
           ("pl" . "perl")
           ("py" . "python")
           ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
           ("rb" . "ruby")
           ("go" . "go run")
           ("js" . "node") ; node.js
           ("ts" . "tsc --alwaysStrict --lib DOM,ES2015,DOM.Iterable,ScriptHost --target ES5") ; TypeScript
           ("sh" . "bash")
           ("clj" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
           ("rkt" . "racket")
           ("ml" . "ocaml")
           ("vbs" . "cscript")
           ("tex" . "pdflatex")
           ("latex" . "pdflatex")
           ("java" . "javac")
           ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
           ))
        -fname
        -fSuffix
        -prog-name
        -cmd-str)
    (when (not (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))
    (setq -fname (buffer-file-name))
    (setq -fSuffix (file-name-extension -fname))
    (setq -prog-name (cdr (assoc -fSuffix -suffix-map)))
    (setq -cmd-str (concat -prog-name " \""   -fname "\""))
    (cond
     ((string-equal -fSuffix "el") (load -fname))
     ((string-equal -fSuffix "java")
      (progn
        (shell-command -cmd-str "*ydai-run-current-file output*" )
        (shell-command
         (format "java %s" (file-name-sans-extension (file-name-nondirectory -fname))))))
     (t (if -prog-name
            (progn
              (message "Running…")
              (shell-command -cmd-str "*ydai-run-current-file output*" ))
          (message "No recognized program file suffix for this file."))))))





;;; cpp run
;; Helper for compilation. Close the compilation window if
;; there was no error at all.

(setq compilation-finish-functions 'compile-autoclose)
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
         (bury-buffer "*compilation*")
         (winner-undo)
         (message "Build successful."))
        (t
         (message "Compilation exited abnormally: %s" string))))

(defun cpp-single-file-compile ()
  (interactive)
  (compile
   (concat "g++ -g " (buffer-file-name) " -o " (file-name-sans-extension (buffer-file-name))))
  (sleep-for 1) ;; this is very important
  (shell-command (concat "./" (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))))


;;; set for tab
(setq-default indent-tabs-mode nil) ; emacs 23.1, 24.2, default to t
(defun my-insert-tab-char ()
  "Insert a tab char. (ASCII 9, \t)."
  (interactive)
  (insert "\t"))


;;; for c c++ mode
;;; http://stackoverflow.com/questions/663588/emacs-c-mode-incorrect-indentation
(defun my-c-mode-common-hook ()
  "My customizations for all of 'c-mode', 'c++-mode', 'objc-mode', 'java-mode'."
  (c-set-offset 'substatement-open 0)
  ;; other customizations can go here

  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)                  ;; Default is 2
  (setq c-indent-level 4)                  ;; Default is 2

  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode t)  ; use spaces only if nil
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


;;; set nyan mode---cat



;;; swith frame
(global-set-key (kbd "s-1") 'other-frame)
(global-set-key (kbd "s-2") 'prev-frame)
(global-set-key (kbd "s-3") 'new-frame)
(global-set-key (kbd "s-4") 'delete-frame)

(defun prev-frame ()
  (interactive)
  (other-frame -1))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))



;;;============settings=====================
(set-face-attribute 'default nil :font "Monaco-18" )
(setq-default cursor-type 'bar)
;;; show the line number
(global-linum-mode t)
(nyan-mode t)

(global-set-key (kbd "TAB") 'my-insert-tab-char) ; same as Ctrl+i
(setq-default tab-width 4) ; emacs 23.1, 24.2, default to 8
(setq tab-width 4)
(setq-default tab-always-indent t)
(setq-default tab-always-indent nil)
(setq-default tab-always-indent 'complete)

(global-set-key (kbd "<f8>") 'ydai-run-current-file)
(add-hook 'c++-mode-hook (lambda () (local-set-key (kbd "C-c C-c") 'cpp-single-file-compile)))



;; search Wikipedia
(require 'browse-url) ; part of gnu emacs

(defun my-lookup-wikipedia ()
  "Look up the word under cursor in Wikipedia.
If there is a text selection (a phrase), use that.

This command switches to browser."
  (interactive)
  (let (word)
    (setq word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word)))
    (setq word (replace-regexp-in-string " " "_" word))
    (browse-url (concat "http://en.wikipedia.org/wiki/" word))
    ;; (eww myUrl) ; emacs's own browser
    ))


(defun my-select-inside-quotes ()
  "Select text between double straight quotes
on each side of cursor."
  (interactive)
  (let (p1 p2)
    (skip-chars-backward "^\"")
    (setq p1 (point))
    (skip-chars-forward "^\"")
    (setq p2 (point))
    (goto-char p1)
    (push-mark p2)
    (setq mark-active t)))


;; let use type special char more quickly

(defvar *unshifted-special-chars-layout*
  '(                           ; from -> to
    ("8" "%")
    ("7" "&")
    ("9" "-")
    ("0" "=")

    ("%" "8")
    ("&" "7")
    ("-" "9")
    ("=" "0")))

(defun mb-str-to-unibyte-char (s)
  "Translate first multibyte char in s to internal unibyte representation."
  (multibyte-char-to-unibyte (string-to-char s)))

(defun remap-keyboard (mapping)
  "Setup keyboard translate table using a list of pairwise key-mappings."
  (mapcar
   (lambda (mb-string-pair)
     (apply #'keyboard-translate
            (mapcar #'mb-str-to-unibyte-char mb-string-pair)))
   mapping))

(remap-keyboard *unshifted-special-chars-layout*)

;; ==============
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))


(local-set-key "%" 'match-paren)

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))




(global-set-key (kbd "C-w") 'whole-line-or-region-kill-ring-save)
(global-set-key (kbd "M-w") 'whole-line-or-region-kill-region)



;; insert a link
(defun link (link description)
  (interactive "sLink:\nsDescription:")
  (insert (concat "[[" link "][" description "]]")))


(setq org-html-checkbox-type 'html)

(provide 'init-locales)
;;; init-locales.el ends here
