;;; appearance --- config for emacs appearance
;;; Commentary:

;;; Code:

(require 'idle-highlight-mode)
(require 'nyan-mode)
(require 'golden-ratio)
(require 'diminish)
(require 'anzu)
;; (require 'eyebrowse)

;;; custom theme
(if (window-system)
    (progn
      (setq custom-theme-directory "~/.emacs.d/lib/themes/")
      (add-to-list 'custom-theme-load-path custom-theme-directory)
      (load-theme 'tmtxt-dark t))
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
                golden-ratio-mode
                ))
  (when (fboundp mode) (funcall mode 1)))
(golden-ratio-toggle-widescreen)
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)
(setq golden-ratio-adjust-factor 0.82)
(golden-ratio)

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
 golden-ratio-exclude-modes '("ediff-mode")
 )


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


;;; default font
(set-default-font "Hack")


;;; watch words
(defun tmtxt-add-watchwords ()
  "Add watch words"
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'tmtxt-add-watchwords)

;;; diminish
(dolist (d '((yas-minor-mode             "яс"   yasnippet)
             (paredit-mode               "(П)"  paredit)
             (elisp-slime-nav-mode       ""     elisp-slime-nav)
             (magit-auto-revert-mode     ""     magit)
             (helm-mode                  ""     helm-mode)
             (highlight-parentheses-mode ""     highlight-parentheses)
             (projectile-mode            ""     projectile)
             (autopair-mode              ""     autopair)
             (subword-mode               "")
             (eldoc-mode                 "")
             (anzu-mode                  ""     anzu)
             (auto-fill-function " ⏎")
             (golden-ratio-mode          "ф"    golden-ratio)
             (undo-tree-mode             "⌘-Z"  undo-tree)
             ))
  (destructuring-bind (mode display &optional feature) d
    (if feature
        (eval-after-load feature
          `(diminish ',mode ,display))
      (diminish mode display))))

;;; anzu
(global-anzu-mode +1)

;;; eyebrowse
;; (eyebrowse-mode t)
;; (eyebrowse-setup-opinionated-keys)

;;; finally, provide the library
(provide 'tmtxt-appearance)
;;; tmtxt-appearance.el ends here
