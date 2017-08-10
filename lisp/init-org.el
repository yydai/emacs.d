(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

;; Various preferences
(setq org-log-done t
      org-edit-timestamp-down-means-later t
      org-archive-mark-done nil
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)


;; Lots of stuff from http://doc.norang.ca/org-mode.html

(defun sanityinc/grab-ditaa (url jar-name)
  "Download URL and extract JAR-NAME as `org-ditaa-jar-path'."
  ;; TODO: handle errors
  (message "Grabbing " jar-name " for org.")
  (let ((zip-temp (make-temp-name "emacs-ditaa")))
    (unwind-protect
        (progn
          (when (executable-find "unzip")
            (url-copy-file url zip-temp)
            (shell-command (concat "unzip -p " (shell-quote-argument zip-temp)
                                   " " (shell-quote-argument jar-name) " > "
                                   (shell-quote-argument org-ditaa-jar-path)))))
      (when (file-exists-p zip-temp)
        (delete-file zip-temp)))))

(after-load 'ob-ditaa
  (unless (and (boundp 'org-ditaa-jar-path)
               (file-exists-p org-ditaa-jar-path))
    (let ((jar-name "ditaa0_9.jar")
          (url "http://jaist.dl.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip"))
      (setq org-ditaa-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
      (unless (file-exists-p org-ditaa-jar-path)
        (sanityinc/grab-ditaa url jar-name)))))



(define-minor-mode prose-mode
  "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
  nil " Prose" nil
  (if prose-mode
      (progn
        (setq truncate-lines nil)
        (setq word-wrap t)
        (setq cursor-type 'bar)
        (when (eq major-mode 'org)
          (kill-local-variable 'buffer-face-mode-face))
        (buffer-face-mode 1)
        ;;(delete-selection-mode 1)
        (set (make-local-variable 'blink-cursor-interval) 0.6)
        (set (make-local-variable 'show-trailing-whitespace) nil)
        (flyspell-mode 1)
        (when (fboundp 'visual-line-mode)
          (visual-line-mode 1)))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'show-trailing-whitespace)
    (buffer-face-mode -1)
    ;; (delete-selection-mode -1)
    (flyspell-mode -1)
    (when (fboundp 'visual-line-mode)
      (visual-line-mode -1))))

;;(add-hook 'org-mode-hook 'buffer-face-mode)


(setq org-support-shift-select t)

;;; Capturing

(global-set-key (kbd "C-c c") 'org-capture)



(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => org-default-notes-file
         "* NEXT %?\n%U\n" :clock-resume t)
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
        ))

;;; my own settings




;;; Refiling

(setq org-refile-use-cache nil)

; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

;; Exclude DONE state tasks from refile targets
(defun sanityinc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

(defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

(defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-agenda-refile goto rfloc no-update)))

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)


;;; To-do settings
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

;; Changing a task state is done with C-c C-t KEY
;; where KEY is the appropriate fast todo state selection key as defined in org-todo-keywords.
(setq org-use-fast-todo-selection t)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;;; Adding New Tasks Quickly with Org Capture
;;; This will replace the "remember"


;;; Agenda views



(let ((active-project-match "-INBOX/PROJECT"))

  (setq org-stuck-projects
        `(,active-project-match ("NEXT")))

  (setq org-agenda-compact-blocks t
        org-agenda-sticky t
        org-agenda-start-on-weekday nil
        org-agenda-span 'day
        org-agenda-include-diary nil
        org-agenda-sorting-strategy
        '((agenda habit-down time-up user-defined-up effort-up category-keep)
          (todo category-up effort-up)
          (tags category-up effort-up)
          (search category-up))
        org-agenda-window-setup 'current-window
        org-agenda-custom-commands
        `(("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("g" "GTD"
           ((agenda "" nil)
            (tags "INBOX"
                  ((org-agenda-overriding-header "Inbox")
                   (org-tags-match-list-sublevels nil)))
            (stuck ""
                   ((org-agenda-overriding-header "Stuck Projects")
                    (org-agenda-tags-todo-honor-ignore-options t)
                    (org-tags-match-list-sublevels t)
                    (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "-INBOX/NEXT"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        ;; TODO: skip if a parent is WAITING or HOLD
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(todo-state-down effort-up category-keep))))
            (tags-todo ,active-project-match
                       ((org-agenda-overriding-header "Projects")
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX/-NEXT"
                       ((org-agenda-overriding-header "Orphaned Tasks")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        ;; TODO: skip if a parent is a project
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                                (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/WAITING"
                       ((org-agenda-overriding-header "Waiting")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/DELEGATED"
                       ((org-agenda-overriding-header "Delegated")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX/HOLD"
                       ((org-agenda-overriding-header "On Hold")
                        ;; TODO: skip if a parent is WAITING or HOLD
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            ;; (tags-todo "-NEXT"
            ;;            ((org-agenda-overriding-header "All other TODOs")
            ;;             (org-match-list-sublevels t)))
            )))))


(add-hook 'org-agenda-mode-hook 'hl-line-mode)


;;; Org clock

;; Save the running clock and all clock history when exiting Emacs, load it on startup




;;; Show the clocked-in task - if any - in the header line






;; Remove empty LOGBOOK drawers on clock out

;; TODO: warn about inconsistent items, e.g. TODO inside non-PROJECT
;; TODO: nested projects!



;;; Archiving

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archive")





(require-package 'org-pomodoro)
(setq org-pomodoro-keep-killed-pomodoro-time t)
(after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
  (define-key org-agenda-mode-map "\C-n" 'next-line)
  (define-key org-agenda-keymap "\C-n" 'next-line)
  (define-key org-agenda-mode-map "\C-p" 'previous-line)
  (define-key org-agenda-keymap "\C-p" 'previous-line))


;;;=====================

;;;=====================
(require 'remember)

(add-hook 'remember-mode-hook 'org-remember-apply-template)

(define-key global-map [(control meta ?r)] 'remember)
(custom-set-variables
 '(org-agenda-files (quote ("~/workspace/github/org/gtd/todo.org")))
 '(org-default-notes-file "~/workspace/github/org/gtd/notes.org")
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates
   (quote ((116 "* TODO %?\n  %u" "~/workspace/github/org/gtd/todo.org" "Tasks")
           (110 "* %u %?" "~/workspace/github/org/gtd/notes.org" "Notes"))))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler))))



;; ;; Show iCal calendars in the org agenda
;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
;;   (setq org-agenda-include-diary t
;;         org-agenda-custom-commands
;;         '(("I" "Import diary from iCal" agenda ""
;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;             (lambda ()
;;               (goto-char (point-min))
;;               (save-excursion
;;                 (while (re-search-forward "^[a-z]" nil t)
;;                   (goto-char (match-beginning 0))
;;                   (insert "0:00-24:00 ")))
;;               (while (re-search-forward "^ [a-z]" nil t)
;;                 (goto-char (match-beginning 0))
;;                 (save-excursion
;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;                 (insert (match-string 0))))))



;; disable keybinding for org mode


(after-load 'org
  (define-key org-mode-map (kbd "C-,") nil)
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "M-h") nil)
    (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

(after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   `(
     (clojure . t)
     (C . t)
     (ditaa . t)
     (dot . t)
     (ditaa . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (http . t)
     (ipython . t)
     (js . t)
     (ledger . t)
     (latex . t)
     (org . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (R . t)
     (shell . t)
     (sql . t)
     (scheme . t)
     (sqlite . t))))

;;----------------------------------------------------------------------------------------------------
;; Org blog settings
;;----------------------------------------------------------------------------------------------------
(require 'ox-publish)
(require 'ox-html)


(setq org-publish-project-alist
      '(
        ("blog-notes"
         :base-directory "~/workspace/blog/org/"
         :base-extension "org"
         :publishing-directory "~/workspace/blog/public_html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 2
         :section-numbers nil
         :html-mathjax-template "<script type=\"text/javascript\" async src=\"https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML\"></script>"
         )
        ("blog-static"
         :base-directory "~/workspace/blog/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|jpeg"
         :publishing-directory "~/workspace/blog/public_html/"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("blog" :components ("blog-notes" "blog-static"))

        ))

;; add jquery support
(setq org-html-head-extra
      "<script src='https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js'></script>
<link rel='stylesheet' href='../css/worg2.css' typbe='text/css'/>
<link rel='shortcut icon' type='image/x-icon' href='/favicon.ico'>")

(setq org-html-preamble "
<div class='nav'>
<div class='blog' style='text-align:right'>
<a href='/index.html'> Home </a> | <a href='/contact.html'> About </a>
</div>
</div>")

(setq org-html-postamble "<script type=\"text/javascript\" src=\"https://cdn.bootcss.com/jquery/3.2.1/jquery.min.js\"></script>
<script type=\"text/javascript\" src=\"https://cdnjs.cloudflare.com/ajax/libs/datejs/1.0/date.min.js\"></script>

<script>
var base_url = 'https://api.github.com';
var title = document.title;
var owner = 'yydai';
var repo = 'yydai.github.io';
var search_issues = base_url + '/search/issues?q=' + title + '+user:' + owner + '+label:blog'+ '+state:open';

console.log(\"search_issues = \"+ search_issues);

function test() {
  jQuery.ajax({
      type: 'GET',
      async: false,
      dataType:'json',
      url: search_issues,
      success:function(data) {
         result = data;
      }
  });
  return result;
}
var result = test();
var items = result.items[0];
if(jQuery.isEmptyObject(items)) {
    create(title);
} else {
    html_url = items.html_url;
	document.body.innerHTML +=
'<div id=\"comments\"><h2>Comments</h2><div id=\"header\">Want to leave a comment? Visit <a href=\"'+ html_url + '\"> this issue page on GitHub</a> (you will need a GitHub account).</div></div>'
}


function create(title) {
	var create_url = 'https://blog-api-server.herokuapp.com/issues?title=' + title + '&labels=blog&body=Welcome to leave comments here.&owner=yydai&repo=yydai.github.io&auth=eXlkYWk6ZGVpc3Q5MjgxNw=='

	jQuery.ajax({
      type: 'GET',
      async: false,
      dataType:'json',
      url: create_url,
      success:function(data) {
         result = data;
      }
  });
}


console.log(\"total_count = \" + result.total_count);
if(result.total_count == 1) {
    var comments_url = result.items[0].comments_url;
} else if (result.total_count == 0) {
        // create a new issue
    //create(title);
} else {
        // result not only
        alert('Cannot load the comments.');
}

function loadComments(data) {
	repo = 'github.com'
    for (var i=0; i<data.length; i++) {
      var cuser = data[i].user.login;
      var cuserlink = 'https://' + repo + '/' + data[i].user.login;
      var cbody = data[i].body_html;
      var cavatarlink = data[i].user.avatar_url;
      var cdate = Date.parse(data[i].created_at).toString('yyyy-MM-dd HH:mm:ss');

	  var html_url = items.html_url + '#issuecomment-' + data[i].url.substring(data[i].url.lastIndexOf('/')+1);

      var code = '<div class=\"comment\"><div class=\"commentheader\"><div class=\"commentgravatar\">' + '<img src=\"' + cavatarlink + '\" alt=\"\" width=\"20\" height=\"20\">' + '</div><a class=\"commentuser\" href=\"'+ cuserlink + '\">' + cuser + '</a><a class=\"commentdate\" href=\"' + html_url + '\">' + cdate + '</a></div><div class=\"commentbody\">' + cbody + '</div></div>';

      $('#comments').append(code);
    }
  }


var comments_api = comments_url + '?per_page=100';
console.log(\"comments api: \" + comments_api);
$.ajax(comments_api, {
    headers: {Accept: 'application/vnd.github.full+json'},
    dataType: 'json',
    success: function(msg){
      loadComments(msg);
   }
  });


</script>

<hr />\n <div class='footer'>
© 2017 yydai<br/>
Email: dai92817@icloud.com
</div>")



;; this code can clear the cache and will regenerate all the html files
;; (setq org-publish-use-timestamps-flag nil)
;;; screen shot
;;; https://emacs-china.org/t/org-mode/79
(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-directory (buffer-file-name))
                  "/imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
                                        ; take screenshot
  (if (eq system-type 'darwin)
      (progn
        (call-process-shell-command "screencapture" nil nil nil nil " -s " (concat
                                                                            "\"" filename "\"" ))
        (call-process-shell-command "convert" nil nil nil nil (concat "\"" filename "\" -resize  \"50%\"" ) (concat "\"" filename "\"" ))
        ))

  (setq relative-dir (concat "./imgs/" (file-name-nondirectory filename)))
  (if (file-exists-p filename)
      (insert (concat "#+ATTR_HTML: :width 100%\n[[file:" relative-dir "]]")))
  (org-display-inline-images)
  )


;; 下面解决使用 M-q 自动断行，会有空格的问题
(defun clear-single-linebreak-in-cjk-string (string)
  "clear single line-break between cjk characters that is usually soft line-breaks"
  (let* ((regexp "\\([\u4E00-\u9FA5]\\)\n\\([\u4E00-\u9FA5]\\)")
         (start (string-match regexp string)))
    (while start
      (setq string (replace-match "\\1\\2" nil nil string)
            start (string-match regexp string start))))
  string)

(defun ox-html-clear-single-linebreak-for-cjk (string backend info)
  (when (org-export-derived-backend-p backend 'html)
    (clear-single-linebreak-in-cjk-string string)))

(add-to-list 'org-export-filter-final-output-functions
             'ox-html-clear-single-linebreak-for-cjk)

;; The codes of blow are coming from this place:
;; http://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html
;; insert a youtube video link, show it can show in my blog
;; we can use it by this [[yt:A3JAlWM8qRM]]
(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))


;; set org bullets
;; https://zhangda.wordpress.com/
;; https://github.com/sabof/org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))



;; http://kitchingroup.cheme.cmu.edu/blog/2015/07/10/Drag-images-and-files-onto-org-mode-and-insert-a-link-to-them/
;; drag file to org mode
(defun my-dnd-func (event)
  (interactive "e")
  (goto-char (nth 1 (event-start event)))
  (x-focus-frame nil)
  (let* ((payload (car (last event)))
         (type (car payload))
         (fname (cadr payload))
         (img-regexp "\\(png\\|jp[e]?g\\|svg\\)\\>"))
    (cond
     ;; insert image link
     ((and  (eq 'drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (dired-copy-file fname (format "./imgs/%s" (file-name-nondirectory fname)) t)
      (insert "#+ATTR_HTML: :width 100%\n")
      (insert (format "[[%s]]" (format "./imgs/%s" (file-name-nondirectory fname))))
      (org-display-inline-images t t))
     ;; insert image link with caption
     ((and  (eq 's-drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert "#+ATTR_ORG: :width 300\n")
      (insert (concat  "#+CAPTION: " (read-input "Caption: ") "\n"))
      (insert (format "[[%s]]" fname))
      (org-display-inline-images t t))
     ;; C-drag-n-drop to open a file
     ((and  (eq 's-drag-n-drop (car event))
            (eq 'file type))
      (find-file fname))
     ((and (eq 'M-drag-n-drop (car event))
           (eq 'file type))
      (insert (format "[[attachfile:%s]]" fname)))
     ;; regular drag and drop on file
     ((eq 'file type)
      (insert (format "[[%s]]\n" fname)))
     (t
      (error "I am not equipped for dnd on %s" payload)))))


(define-key org-mode-map (kbd "<drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<s-drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<M-drag-n-drop>") 'my-dnd-func)


;; insert github gist codes
(defun insert-gist (link)
  (interactive "sEmbed link is?")
  (insert (format "#+BEGIN_EXPORT html
	%s
	#+END_EXPORT" link)))

(add-hook 'org-mode-hook #'xah-math-input-mode-on)

(provide 'init-org)
;;; init-org.el ends here
