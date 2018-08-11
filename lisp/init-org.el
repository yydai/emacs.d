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

;; org babel settings
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
     (go . t)
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


;; org blog settings
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
<link href='http://apps.bdimg.com/libs/highlight.js/9.1.0/styles/zenburn.min.css' rel='stylesheet'>
<script src='http://apps.bdimg.com/libs/highlight.js/9.1.0/highlight.min.js'></script>
<script>hljs.initHighlightingOnLoad();</script>
<link rel='stylesheet' href='../css/worg2.css' typbe='text/css'/>
<link rel='shortcut icon' type='image/x-icon' href='/favicon.ico'>
<!-- Global site tag (gtag.js) - Google Analytics -->
<script async src='https://www.googletagmanager.com/gtag/js?id=UA-111585106-1'></script>
<script>
window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());

gtag('config', 'UA-111585106-1');
</script>")

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
  ;;(org-display-inline-images)
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
  ;; copy image file to publish dir, so I don't need to move the image by command
  ;; This place has a bug, I need to know the current folder
  ;; (dired-copy-file filename
  ;;                  (format "~/workspace/blog/public_html/%s/imgs/%s"
  ;;                          (car (last (butlast (split-string (file-name-directory filename) "\/") 3)))
  ;;                          (file-name-nondirectory filename)) t)

  (if (file-exists-p filename)
      (insert (concat "#+ATTR_HTML: :width 80%\n[[file:" relative-dir "]]")))
  ;;(org-display-inline-images)
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
(defvar yt-iframe-formats
  ;; You may want to change your width and height.
  (concat "<iframe width=\"560\""
          " height=\"335\""
          " style=\"display:block;margin: auto\""
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
     (html (format yt-iframe-formats
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))


;; set org bullets
;; https://zhangda.wordpress.com/
;; https://github.com/sabof/org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 0)))


;; http://kitchingroup.cheme.cmu.edu/blog/2015/07/10/Drag-images-and-files-onto-org-mode-and-insert-a-link-to-them/
;; drag file to org mode
(defun my-dnd-func (event)
  (interactive "e")
  (goto-char (nth 1 (event-start event)))
  (x-focus-frame nil)
  (let* ((payload (car (last event)))
         (type (car payload))
         (fname (cadr payload))
         (img-regexp "\\(png\\|jp[e]?g\\|svg\\|gif\\)\\>"))
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

;; xah math input
;; for more information please visit here:http://ergoemacs.org/emacs/xmsi-math-symbols-input.html
(add-hook 'org-mode-hook #'xah-math-input-mode-on)

;; From this place, I will set up my GTD system
;; C-ca org-agenda
(define-key global-map "\C-cc" 'org-capture)

;; C-'
(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/gtd.org"
                         "~/gtd/tickler.org"
                         "~/gtd/someday.org"))


(defun verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'verify-refile-target)

(setq org-agenda-dir "~/workspace/gtd")

(setq org-refile-targets '((nil :maxlevel . 5)
                           ("~/gtd/gtd.org" :maxlevel . 3)
                           ("~/gtd/someday.org" :level . 1)
                           ("~/gtd/tickler.org" :maxlevel . 2)))

;; TODO entry to automatically change to DONE when all children are done
;; code from: http://orgmode.org/manual/Breaking-down-tasks.html#Breaking-down-tasks
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
;; end


(setq org-agenda-span 'day)
(setq org-todo-keywords '((sequence "TODO(t)" "DOING(s)" "BLOCKED" "REVIEW" "|" "DONE(d)")))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("DOING" :foreground "yellow" :weight bold)
              ("BLOCKED" :foreground "red" :weight bold)
              ("REVIEW" :foreground "orange" :weight bold)
              )))
;; Fast todo selection allows changing from any task todo state to any other state directly
;; by selecting the appropriate key from the fast todo selection key menu.
(setq org-use-fast-todo-selection t)

;; Then each time you turn an entry from a TODO (not-done) state into any of the DONE
;; states, a line ‘CLOSED: [timestamp]’ will be inserted just after the headline.
;; http://orgmode.norg/manual/Closing-items.html#Closing-items
(setq org-log-done 'time)

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "DOING")

(setq org-clock-out-switch-to-state "DONE")

;; tags
(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h)
                      ("@homework" . ?o)
                      ("@buy" . ?b)
                      ("@study" . ?s)))


;; clock
;;To save the clock history across Emacs sessions, use
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; http://orgmode.org/manual/Resolving-idle-time.html#Resolving-idle-time
;;(setq org-clock-idle-time 1000)

;; start capture
(setq org-default-notes-file (concat org-directory "~/workspace/gtd/notes.org"))
`
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))


;; In order to include entries from the Emacs diary into Org mode's agenda
(setq org-agenda-include-diary t
      diary-file (locate-user-emacs-file "~/gtd/diary.org")
      org-agenda-diary-file 'diary-file)


(setq org-stuck-projects
      '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))
(setq org-agenda-window-setup 'current-window)

;; diary for chinese birthday
;; https://emacs-china.org/t/topic/2119/14
(defun my--diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
  (if year
      (let* ((d-date (diary-make-date lunar-month lunar-day year))
             (a-date (calendar-absolute-from-gregorian d-date))
             (c-date (calendar-chinese-from-absolute a-date))
             (cycle (car c-date))
             (yy (cadr c-date))
             (y (+ (* 100 cycle) yy)))
        (diary-chinese-anniversary lunar-month lunar-day y mark))
    (diary-chinese-anniversary lunar-month lunar-day year mark)))

(require 'cal-china-x)
(setq mark-holidays-in-calendar t)
(setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
(setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
;; (setq calendar-holidays
;;       (append cal-china-x-important-holidays
;;               cal-china-x-general-holidays
;;               other-holidays))

;; pomodoro 通知功能
(defun notify-osx (title message)
  (call-process "terminal-notifier"
                nil 0 nil
                "-group" "Emacs"
                "-title" title
                "-message" message
                ;;"-sender" "org.gnu.Emacs"
                "-activate" "oeg.gnu.Emacs"))



(add-hook 'org-pomodoro-finished-hook
          (lambda ()
            (notify-osx "Pomodoro completed!" "Time for a break.")))

(add-hook 'org-pomodoro-break-finished-hook
          (lambda ()
            (notify-osx "Pomodoro Short Break Finished" "Ready for Another?")))

(add-hook 'org-pomodoro-long-break-finished-hook
          (lambda ()
            (notify-osx "Pomodoro Long Break Finished" "Ready for Another?")))

(add-hook 'org-pomodoro-killed-hook
          (lambda ()
            (notify-osx "Pomodoro Killed" "One does not simply kill a pomodoro!")))

;; 任务提醒功能
;; https://emacs-china.org/t/org-agenda/232
(require 'appt)
(setq appt-time-msg-list nil)    ;; clear existing appt list
(setq appt-display-interval '5)  ;; warn every 5 minutes from t - appt-message-warning-time
(setq
 appt-message-warning-time '15  ;; send first warning 15 minutes before appointment
 appt-display-mode-line nil     ;; don't show in the modeline
 appt-display-format 'window)   ;; pass warnings to the designated window function
(appt-activate 1)                ;; activate appointment notification
(display-time)                   ;; activate time display

(org-agenda-to-appt)             ;; generate the appt list from org agenda files on emacs launch
(run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view

(defun my-appt-display (min-to-app new-time msg)
  (notify-osx
   (format "Appointment in %s minutes" min-to-app)    ;; passed to -title in terminal-notifier call
   (format "%s" msg)))                                ;; passed to -message in terminal-notifier call
(setq appt-disp-window-function (function my-appt-display))

;; like search study, we can use C-c a s s command
;; http://sachachua.com/blog/2013/06/how-i-use-emacs-org-mode-for-my-weekly-reviews/
(setq org-agenda-custom-commands
      '(("x" agenda)
        ("d" todo "DONE")
        ("W" todo-tree "BLOCKED")
        ("gm" tags "@game")
        ("w" "Weekly Review"
         ((agenda "" ((org-agenda-ndays 7))) ;; review upcoming deadlines and appointments
          (todo "TODO") ;; review all projects (assuming you use todo keywords to designate projects)
          (todo "BLOCKED") ;; review someday/maybe items
          (todo "DONE")))
        ("v" tags-todo "+boss-urgent")
        ("U" tags-tree "+boss-urgent")
        ("f" occur-tree "\\<FIXME\\>")
        ("h" . "HOME+Name tags searches") ; description for "h" prefix
        ("ss" tags "@study")
        ))

(setq org-clock-out-when-done t)
(setq org-clock-report-include-clocking-task t)
(setq org-clock-continuously 'nil)

(defun archive-when-done ()
  "Archive current entry if it is marked as DONE (see `org-done-keywords')."
  )
(add-hook 'org-after-todo-state-change-hook
          'archive-when-done)


(setq org-archive-location (concat "~/gtd/archive/archive-" (format-time-string "%Y%m" (current-time)) ".org_archive::"))

;; pomodoro setting
(defun pomodoro-start ()
  "Starts and automatically clocks out a Pomodoro unit of 30 minutes."
  (interactive)
  (org-pomodoro)
  (org-clock-in)
  (message "Starting pomodoro cycle of 30 minutes.")
  )


(add-hook 'org-pomodoro-finished-hook
          (lambda ()
            (org-clock-out)))

(add-hook 'org-pomodoro-break-finished-hook
          (lambda ()
            (pomodoro-start)))

(global-set-key '[f5] 'pomodoro-start)

;; simplifying clock-in / clock-out
(global-set-key '[f5] 'org-clock-in)
(global-set-key '[f6] 'org-clock-out)

(provide 'init-org)
;;; init-org.el ends here
