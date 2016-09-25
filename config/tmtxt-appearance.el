;;; appearance --- config for emacs appearance
;;; Commentary:

;;; Code:

(require 'idle-highlight-mode)
(require 'nyan-mode)
(require 'golden-ratio)

;;; custom theme
(if (window-system)
    (progn
      (setq custom-theme-directory "~/.emacs.d/lib/themes/")
      (add-to-list 'custom-theme-load-path custom-theme-directory)
      (load-theme 'truong t))
  (progn
    (load-theme 'manoj-dark)))


;;; enabled modes
(dolist (mode '(mouse-wheel-mode
                nyan-mode
                display-time-mode
                column-number-mode
                show-paren-mode
                global-hl-line-mode
                line-number-mode
                global-linum-mode
                golden-ratio-mode))
  (when (fboundp mode) (funcall mode 1)))
(golden-ratio-toggle-widescreen)
(split-window-right)
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)

;;; disabled modes
(dolist (mode '(menu-bar-mode
                tool-bar-mode
                scroll-bar-mode
                tooltip-mode
                blink-cursor-mode))
  (when (fboundp mode) (funcall mode -1)))


;;; mac os specific
(tmtxt/in '(darwin)
  (setq-default
   ns-auto-hide-menu-bar nil
   ns-use-native-fullscreen nil)
  (set-face-attribute 'default nil :height 120))

;;; linux specific
(tmtxt/in '(gnu/linux)
  (set-face-attribute 'default nil :height 120))

;;; common config
(setq-default

 ;; smooth scroll
 mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;one line at a time
 mouse-wheel-progressive-speed nil      ;don't accelerate scrolling
 mouse-wheel-follow-mouse 't            ;scroll window under mouse
 scroll-step 1                          ;keyboard scroll one line at a time

 tab-width 2
 cursor-type 'bar
 visible-bell nil
 inhibit-startup-message t
 uniquify-buffer-name-style 'forward
 whitespace-style '(face trailing lines-tail tabs)
 indicate-empty-lines t
 split-width-threshold nil
 split-height-threshold nil
 show-trailing-whitespace t
 frame-title-format '(buffer-file-name "%f" ("%b"))

 nyan-bar-length 15
 golden-ratio-exclude-modes '("ediff-mode")
 )


;;; FiraCode font
(when (window-system)
  (set-default-font "Fira Code"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))


;;; same window buffer
(add-to-list 'same-window-buffer-names "*MozRepl*")
(add-to-list 'same-window-buffer-names "*SQL*")
(add-to-list 'same-window-buffer-names "*Help*")
(add-to-list 'same-window-buffer-names "*Apropos*")
(add-to-list 'same-window-buffer-names "*Process List*")


;;; bigger minibuffer text
(defun tmtxt/make-minibuffer-text-bigger ()
  "Make minibuffer text bigger"
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 1.2)))
  (setq line-spacing 0.2))
(add-hook 'minibuffer-setup-hook 'tmtxt/make-minibuffer-text-bigger)

;;; transparent emacs
(defun tmtxt/toggle-alpha ()
  "Make Emacs become transparent"
  (interactive)
  (let ((a (frame-parameter nil 'alpha)))
    (if (or (not (numberp a)) (= a 100))
        (set-frame-parameter nil 'alpha 70)
      (set-frame-parameter nil 'alpha 100))))


;;; finally, provide the library
(provide 'tmtxt-appearance)
;;; tmtxt-appearance.el ends here
