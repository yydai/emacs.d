(require 'simple-httpd)
(require 'browse-url)


(defmacro define-background-function-wrapper (bg-function fn)
  (let ((is-loading-sym (intern (concat "*" (symbol-name bg-function) "-is-loading*"))))
    `(progn
       (defvar ,is-loading-sym nil)
       (defun ,bg-function ()
         (interactive)
         (when ,is-loading-sym
           (message ,(concat (symbol-name fn) " is already loading")))
         (setq ,is-loading-sym t)
         (make-thread (lambda ()
                        (unwind-protect
                            (,fn)
                          (setq ,is-loading-sym nil))))))))

(defun blog-open ()
  (interactive)
  (setq base "~/workspace/blog/org/index.org")
  (find-file base))


;; set the http server
(setq httpd-root "~/workspace/blog/public_html/")
(httpd-serve-directory httpd-root)
(httpd-start)

(defun blog-preview(&optional publish-current)
  "Preivew blog."
  (interactive
   (list (y-or-n-p "Publish current file(y), or publish all(n)?")))

  (if publish-current (org-publish-current-file)
    (org-publish-all))

  (if publish-current
      (progn
        (if (equalp (file-name-nondirectory buffer-file-name) "index.org")
            (setq subpath "/index")
          (setq subpath (substring buffer-file-name (string-match-p
                                                     "/[[:word:]_?]+/[[:word:]-?]+.org$"
                                                     buffer-file-name) (- (length buffer-file-name) 4))))
        (setq url (format "http://%s:%d%s%s" "127.0.0.1" 8080 subpath ".html")))

    (setq url (format "http://%s:%d" "127.0.0.1" 8080)))
  (browse-url url))


(defun blog-stop-preview ()
  (interactive)
  (httpd-stop))


(defun auto-sequence (format start end)
  (interactive "sSequence format is? \nnEnter start number: \nnEnter end number:")
  (progn
    (kmacro-set-format format)
    (kmacro-set-counter start)
    (while (< start (+ 1 end))
      (execute-kbd-macro (read-kbd-macro "<f3> RET"))
      (setq start (+ 1 start)))
    ))

(defun blog-create (title &optional dir)
  "nblog is used to create a new blog in the default directory(~/workspace/blog/org/).
And you should know it needs the blog name and the directory that to restore the file.
reference: https://www.emacswiki.org/emacs/InteractiveFunction
and https://learnxinyminutes.com/docs/elisp/
and http://ergoemacs.org/emacs/elisp_buffer_file_functions.html"

  (interactive "sBlog title to show? \nsDirectory is?")
  (setq base "~/workspace/blog/org/")
  (setq filename
        (concat base dir  "/" title ".org"))
  (if (file-exists-p filename)
      (find-file filename)
    (let ((buf (generate-new-buffer title)))
      (switch-to-buffer buf)
      (goto-char (point-min))
      (insert (concat "#+TITLE: " "\n\n-------\n"))
      (write-file filename)
      (goto-char (+ (length "#+TITLE: ") 1))
      )))


(defun publish-site ()
  "Push the site to github, and then open my blog site."
  (interactive)
  (let ((path "~/workspace/blog/public_html/"))
    (if (= 0 (call-process-shell-command (format "cd %s;make" path)))
        (progn
          (browse-url "http://yydai.github.io")
          (httpd-stop))
      (message "Publish site failed. Please manually do this."))))


(defun blog-site ()
  (interactive)
  (browse-url "https://yydai.github.io"))

(provide 'init-blog)
