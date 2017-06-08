(require 'simple-httpd)
(require 'browse-url)


(defun blog-open ()
  (interactive)
  (setq base "~/workspace/blog/org/index.org")
  (find-file base))


;; set the http server
(setq httpd-root "~/workspace/blog/public_html/")
(httpd-start)

(defun blog-preview(&optional publish-current)
  "Preivew blog."
  (interactive
   (list (y-or-n-p "Publish current file(y), or publish all(n)?")))

  (if publish-current (org-publish-current-file)
    (org-publish-all))

  (httpd-serve-directory httpd-root)
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


(defun blog-create (title &optional dir)
  "nblog is used to create a new blog in the default directory(~/workspace/blog/org/).
And you should know it needs the blog name and the directory that to restore the file.
reference: https://www.emacswiki.org/emacs/InteractiveFunction
and https://learnxinyminutes.com/docs/elisp/
and http://ergoemacs.org/emacs/elisp_buffer_file_functions.html"

  (interactive "sBlog title to show? \nsDirectory is?")
  (setq base "~/workspace/blog/org/")
  (setq filename
        (concat base dir  "/" title))
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
