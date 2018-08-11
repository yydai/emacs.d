(require 'pdf-view)

;;; Code:

(defun pdf-view-scroll-up-or-next-page+ (&optional reverse)
  "Get next page in `pdf-view-mode' buffer in other window.
Optional argument REVERSE default is scroll up (or next page), if REVERSE is non-nil scroll down (or previous page)."
  (interactive)
  (catch 'found
    (walk-windows
     (lambda (w)
       (with-selected-window w
         (when (eq major-mode 'pdf-view-mode)
           (if reverse
               (pdf-view-scroll-down-or-previous-page)
             (pdf-view-scroll-up-or-next-page))
           (throw 'found "Have found")))))))

(defun pdf-view-scroll-down-or-previous-page+ ()
  "Get previous page in `pdf-view-mode' buffer in other window."
  (interactive)
  (pdf-view-scroll-up-or-next-page+ t))

(defadvice scroll-other-window (around pdf-view-scroll-up-or-next-page activate)
  "When next buffer is `pdf-view-mode', do `pdf-view-scroll-up-or-next-page'."
  (other-window +1)
  (if (eq major-mode 'pdf-view-mode)
      (let ((arg (ad-get-arg 0)))
        (if (null arg)
            (pdf-view-scroll-up-or-next-page)
          (pdf-view-next-line-or-next-page arg))
        (other-window -1))
    (other-window -1)
    ad-do-it))

(defadvice scroll-other-window-down (around pdf-view-scroll-down-or-previous-page activate)
  "When next buffer is `pdf-view-mode', do `pdf-view-scroll-down-or-previous-page'."
  (other-window +1)
  (if (eq major-mode 'pdf-view-mode)
      (let ((arg (ad-get-arg 0)))
        (if (null arg)
            (pdf-view-scroll-down-or-previous-page)
          (pdf-view-previous-line-or-previous-page arg))
        (other-window -1))
    (other-window -1)
    ad-do-it))

(defun pdf-view-next-line-or-next-page (arg)
  "Next line if possible, else goto next page."
  (interactive "P")
  (when (= (window-vscroll) (image-next-line (or arg 1)))
    (let ((cur-page (pdf-view-current-page)))
      (pdf-view-next-page)
      (when (/= cur-page (pdf-view-current-page))
        (image-bob)
        (image-bol 1)))))

(defun pdf-view-previous-line-or-previous-page (arg)
  "Previous line if possible, else goto previous page."
  (interactive "P")
  (when (= (window-vscroll) (image-previous-line (or arg 1)))
    (let ((cur-page (pdf-view-current-page)))
      (pdf-view-previous-page)
      (when (/= cur-page (pdf-view-current-page))
        (image-eob)
        (image-bol 1)))))

(defun pdf-view-page-reach-top-p ()
  "Return t if current page have reach top edge, otherwise return nil."
  (equal (window-vscroll) 0))

(defun pdf-view-page-reach-bottom-p ()
  "Return t if current page have reach bottom edge, otherwise return nil."
  (let* ((image (image-get-display-property))
         (edges (window-inside-edges))
         (win-height (- (nth 3 edges) (nth 1 edges)))
         (img-height (ceiling (cdr (image-size image)))))
    (equal img-height (+ win-height (window-vscroll)))))

(provide 'pdf-tools-extension)

;;; pdf-tools-extension.el ends here

;;; LocalWords:  vscroll bol eob img
