(defvar auto-scroll-timer nil
  "Timer for `auto-scroll-mode'.")

(defvar auto-scroll-interval 1000
  "*How many milliseconds to wait before scrolling down one line.
Should be a positive integer.")

(defvar auto-scroll-interval-min 10
  "The minimum milliseconds that scrolling down on line.")

(defvar auto-scroll-amount 50
  "*How many milliseconds to change each time")

(make-variable-buffer-local 'auto-scroll-timer)
(make-variable-buffer-local 'auto-scroll-interval)

(define-minor-mode auto-scroll-mode
  "Scroll down line by line when auto.

\\{auto-scroll-mode-map}"
  :lighter " Scrl"
  (and auto-scroll-timer
       (cancel-timer auto-scroll-timer))
  (when auto-scroll-mode
    (if (< auto-scroll-interval 0)
        (setq auto-scroll-interval
              (default-value 'auto-scroll-interval)))
    (setq auto-scroll-timer
          (run-at-time t (/ auto-scroll-interval 1000.0)
                       'auto-scroll-scroll (current-buffer)))))

(defun auto-scroll-faster (arg)
  (interactive "p")
  (setq auto-scroll-interval (- auto-scroll-interval (* arg auto-scroll-amount)))
  (and (< auto-scroll-interval auto-scroll-interval-min)
       (setq auto-scroll-interval auto-scroll-interval-min))
  (aset auto-scroll-timer 4 (/ auto-scroll-interval 1000.0))
  (message "Scroll at %.2f seconds." (/ auto-scroll-interval 1000.0)))

(defun auto-scroll-slower (arg)
  (interactive "p")
  (auto-scroll-faster (- arg)))

(defun auto-scroll-scroll (buf)
  "Scroll if `auto-scroll-mode' is active."
  (when (eq (current-buffer) buf)
    (condition-case nil
        (funcall (auto-scroll-get-function 'scroll-up) 1)
      (error (auto-scroll-mode -1)))))

(defun auto-scroll-get-function (symbol)
  "Return SYMBOL if it's function is not remapped, else return
the remapping function."
  (or (command-remapping symbol)
      symbol))

(provide 'auto-scroll)
