;;; please reference:
;;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-elpa.el

(require 'package)


;; Set it to `t' to use safer HTTPS to download packages
(defvar melpa-use-https-repo nil
  "By default, HTTP is used to download packages.
But you may use safer HTTPS instead.")

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility

(setq package-user-dir "~/.emacs.d/elpa")


;;; Standard package repositories

(defconst sanityinc/no-ssl (or (< emacs-major-version 24)
                               (and (memq system-type '(windows-nt ms-dos))
                                    (not (gnutls-available-p)))))

;;; use local
;;; how to set--reference:
;;; http://pengpengxp.github.io/2017-07-21-emacs%E4%BD%BF%E7%94%A8%E6%9C%AC%E5%9C%B0package%E6%9D%A5%E5%AE%89%E8%A3%85%E8%BD%AF%E4%BB%B6.html?from=singlemessage&isappinstalled=0
(require 'package)
(setq package-archives '(
                         ("gnu" . "~/.emacs.d/elpa-packages/gnu")
                         ("marmalade" . "~/.emacs.d/elpa-packages/marmalade")
                         ("melpa" . "~/.emacs.d/elpa-packages/melpa")
                         ("emacswiki" . "~/.emacs.d/elpa-packages/emacswiki")
                         ))
(package-initialize)
;;; use Chinese mirrors
(setq package-archives '(("gnu"   . "https://elpa.emacs-china.org/gnu/")
                         ("melpa" . "https://elpa.emacs-china.org/melpa/")
                         ("melpa-stable" . "https://elpa.emacs-china.org/melpa-stable/")
                         ))

;; (setq package-archives
;;       '(("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
;;         ("org-cn"   . "http://elpa.emacs-china.org/org/")
;;         ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")))

;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("melpa" . "http://melpa.milkbox.net/packages/")))

;; NOTE: In case of MELPA problems, the official mirror URL is
;; https://www.mirrorservice.org/sites/stable.melpa.org/packages/





;;; On-demand installation of packages

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))


;;; Fire up package.el

(setq package-enable-at-startup nil)
(package-initialize)



(require-package 'fullframe)
(fullframe list-packages quit-window)


(require-package 'cl-lib)
(require 'cl-lib)

(defun sanityinc/set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (when (> width (length col-name))
    (cl-loop for column across tabulated-list-format
             when (string= col-name (car column))
             do (setf (elt column 1) width))))

(defun sanityinc/maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (sanityinc/set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
      (sanityinc/set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook 'sanityinc/maybe-widen-package-menu-columns)


(provide 'init-elpa)
