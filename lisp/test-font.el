;; 这个配置用于设置 org table 的等宽字体
;; https://emacs-china.org/t/topic/440
(setq fonts
      (cond ((eq system-type 'darwin)     '("Monaco"    "STHeiti"))
            ((eq system-type 'gnu/linux)  '("Menlo"     "WenQuanYi Zen Hei"))
            ((eq system-type 'windows-nt) '("Consolas"  "Microsoft Yahei"))))
(set-face-attribute 'default nil :font
                    (format "%s:pixelsize=%d" (car fonts) 14))
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family (car (cdr fonts)))))
;; Fix chinese font width and rescale
(setq face-font-rescale-alist '(("Microsoft Yahei" . 1.2) ("WenQuanYi Micro Hei Mono" . 1.2) ("STHeiti". 1.2)))
