;;; qunar-job-mode-el -- Major mode for editing Qunar Job files

;; Author: Ying Dai <yingdai@zju.edu.cn>
;; Created:
;; Keywords: Qunar Job major-mode

;; Copyright (C) Ying Dai <yingdai@zju.edu.cn>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Code:

(defvar qunarjob-mode-hook nil)

(defvar qunarjob-mode-map
  (let ((map (make-keymap)))
	(define-key map "\C-j" 'newline-and-indent)
    (define-key map "\C-z" 'func-create)
    map)
  "Keymap for qunarjob major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.job\\'" . qunarjob-mode))

;; syntax highlighting using keywords
;; function return
;; EOF
;; // for shell
;;

(setq qunarjob-highlights
      '(("Sin\\|Cos\\|Sum" . font-lock-function-name-face)
        ("EOF\\|hive_211" . font-lock-constant-face)
        ("function\\|FUNCTION\\|return\\|RETURN\\|select\\|SELECT\\|from\\|FROM\\|where\\|WHERE\\|or\\|OR\\|from\\|FROM\\|between\\|BETWEEN\\|and\\|AND\\|insert\\|INSERT\\|into\\|INTO\\|table\\|TABLE\\|partition\\|PARTITION\\|overwrite\\|OVERWRITE\\|distinct\\|DISTINCT\\|IS\\|not\\|NOT\\|null\\|NULL" . font-lock-keyword-face)
        ("string\\|int" . font-lock-type-face)))

(define-derived-mode qunarjob-mode fundamental-mode "qunarjob-mode"
  "major mode for editing mymath language code."
  (setq font-lock-defaults '(qunarjob-highlights)))

(defconst qunarjob-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; ' is a string delimiter
    (modify-syntax-entry ?' "\"" table)
    ;; " is a string delimiter too
    (modify-syntax-entry ?\" "\"" table)

    ;; / is punctuation, but // is a comment starter
    ;; (modify-syntax-entry ?/ ". 12" table)
    (modify-syntax-entry ?# "<" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    table))

(define-derived-mode qunarjob-mode prog-mode "qunarjob-mode"
  :syntax-table qunarjob-mode-syntax-table
  (font-lock-fontify-buffer))


(defun func-create ()
  "nblog is used to create a new blog in the default directory(~/workspace/blog/org/).
And you should know it needs the blog name and the directory that to restore the file.
reference: https://www.emacswiki.org/emacs/InteractiveFunction
and https://learnxinyminutes.com/docs/elisp/
and http://ergoemacs.org/emacs/elisp_buffer_file_functions.html"

  (interactive)
  (setq pname (file-name-nondirectory (directory-file-name (file-name-directory buffer-file-name))))
  (insert (concat "function " pname " {\n" "hive_211 << EOF\n\n" "EOF\n}"))
  )

;; add the mode to the `features' list
;; (define-key lisp-mode-map (kbd "C-z") 'func-create)

(defun qunarjob-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map qunarjob-mode-map)
  (set-syntax-table qunarjob-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(qunarjob-highlights))
  ;; Register our indentation function
  (setq major-mode 'qunarjob-mode)
  (setq mode-name "qunar")
  (run-hooks 'qunarjob-mode-hook))

(provide 'qunarjob-mode)
