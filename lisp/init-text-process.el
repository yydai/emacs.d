(defcustom text-process-keymap-prefix (kbd "C-c t")
  "Text process keymap prefix."
  :type 'string)

(defvar text-process-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "k") #'kill-current-line)
    (define-key map (kbd "s") #'select-current-line)
    (define-key map (kbd "w") #'copy-current-line)
    (define-key map (kbd "c") #'cut-current-line)
    map)
  "Keymap for text process commands after `text-process-key-prefix'")


(fset 'text-process-command-map text-process-command-map)


(defvar text-process-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map text-process-keymap-prefix 'text-process-command-map)
    map)
  "Keymap for textProcess mode.")

(define-minor-mode text-process-mode
  "This minor mode just a toy. I just want to process text more easily."
  ;; :lighter " TP "
  :keymap text-process-mode-map
  :global t)

(text-process-mode)

(provide 'init-text-process)
