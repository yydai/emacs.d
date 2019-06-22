(require 'company)

(defconst sample-completions
  '("alan" "john" "ada" "don" "hello world" "I love you'"))

(defun company-sample-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (case command
    (interactive (company-begin-backend 'company-sample-backend))
    (prefix (and (eq major-mode 'fundamental-mode)
                 (company-grab-symbol)))
    (candidates
     (remove-if-not
      (lambda (c) (string-prefix-p arg c))
      sample-completions))))

(add-to-list 'company-backends 'company-sample-backend)
