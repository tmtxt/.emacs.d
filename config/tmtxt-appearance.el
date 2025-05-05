(require 'nyan-mode)
(require 'diminish)
(require 'spacemacs-theme)
(require 'zenburn-theme)

;;; custom theme
(load-theme 'zenburn t)


;;; enabled modes
(dolist (mode '(mouse-wheel-mode
                nyan-mode
                display-time-mode
                column-number-mode
                show-paren-mode
                global-hl-line-mode
                global-display-line-numbers-mode))
  (when (fboundp mode) (funcall mode 1)))
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)

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

 nyan-bar-length 10
 )

;;; same window buffer
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

;;; watch words
(defun tmtxt-add-watchwords ()
  "Add watch words"
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'tmtxt-add-watchwords)

;;; diminish, shorter display for minor mode in mode-line
(pcase-dolist
    (`(,mode ,display ,feature)
     '((yas-minor-mode             "яс"   yasnippet)
       (paredit-mode               "(П)"  paredit)
       (elisp-slime-nav-mode       ""     elisp-slime-nav)
       (magit-auto-revert-mode     ""     magit)
       (helm-mode                  ""     helm-mode)
       (highlight-parentheses-mode ""     highlight-parentheses)
       (projectile-mode            ""     projectile)
       (subword-mode               "")
       (eldoc-mode                 "")
       (auto-fill-function " ⏎")))
  (if feature
      (eval-after-load feature `(diminish ',mode ,display))
    (diminish mode display)))

(provide 'tmtxt-appearance)
