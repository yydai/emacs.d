(eval-when-compile
  (defvar url-http-end-of-headers)
  (defvar url-http-response-status))

(require 'url)
(require 'image-file)

(defun image-download-callback (url _status)
  (goto-char url-http-end-of-headers)
  (while (re-search-forward "src=\"\\([^\"]+\\)\"" nil t)
    (let ((src (match-string-no-properties 1)))
      (unless (string-prefix-p "http" src)
        (setq src (concat url src)))
      (when (member (file-name-extension src) image-file-name-extensions)
        (message "Download %s" src)
        (url-retrieve
         src
         (lambda (_status)
           (if (/= url-http-response-status 200)
               (message "Failed download: %s" url)
             (goto-char (point-min))
             (when (re-search-forward "\r?\n\r?\n" nil t)
               (let ((image (buffer-substring (point) (point-max)))
                     (name (file-name-nondirectory src)))
                 (message "Save as %s" name)
                 (with-temp-file name
                   (let ((coding-system-for-write 'binary))
                     (insert image))))))))))))

;;;###autoload
(defun image-download (url)
  (interactive
   (list (read-string "URL: ")))
  (url-retrieve url (lambda (status)
                      (image-download-callback url status)) nil t))

(provide 'image-downloader)
