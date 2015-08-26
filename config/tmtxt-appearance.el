;;; how my emacs appears
;;; this file should be loaded after other files if want to override custom face

(require 'tmtxt-util)
(require 'idle-highlight-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load my custom theme

(if (window-system)
    (progn
      (setq custom-theme-directory "~/.emacs.d/lib/themes/")
      (add-to-list 'custom-theme-load-path custom-theme-directory)
      (load-theme 'tmtxt t))
  (progn
    (load-theme 'manoj-dark)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bigger minibuffer text
(defun tmtxt/minibuffer-setup ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 1.2)))
  (setq line-spacing 0.2))
(add-hook 'minibuffer-setup-hook 'tmtxt/minibuffer-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transparent emacs
(defun tmtxt/toggle-alpha ()
  (interactive)
  (let ((a (frame-parameter nil 'alpha)))
    (if (or (not (numberp a)) (= a 100))
        (set-frame-parameter nil 'alpha 70)
      (set-frame-parameter nil 'alpha 100))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mac os specific
(tmtxt/in '(darwin)
  (set-frame-font  "Monaco-12")			;set the font to support unicode
  (setq ns-auto-hide-menu-bar nil)		;hide mac menu bar
  (setq ns-use-native-fullscreen nil)
  ;; this works on emacs 24.4
  ;; (when (>= (string-to-number emacs-version) 24.4)
  ;;   (set-frame-position (selected-frame) 0 -22)
  ;;   (add-hook 'after-init-hook (lambda () (set-frame-size (selected-frame) 1655 1046 t))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; linux specific
(tmtxt/in '(gnu/linux)
  (set-face-attribute 'default nil :height 120))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; smooth scroll
;;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; display config and mouse support for GUI emacs
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green4")
     (set-face-foreground 'magit-diff-del "red3")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nyan-mode
(tmtxt/set-up 'nyan-mode
  (nyan-mode 1)
  (setq nyan-bar-length 15))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; golden ratio
(tmtxt/set-up 'golden-ratio
  (setq golden-ratio-exclude-modes '("ediff-mode"))
  (golden-ratio-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; same window buffer
(add-to-list 'same-window-buffer-names "*MozRepl*")
(add-to-list 'same-window-buffer-names "*SQL*")
(add-to-list 'same-window-buffer-names "*Help*")
(add-to-list 'same-window-buffer-names "*Apropos*")
(add-to-list 'same-window-buffer-names "*Process List*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'prog-mode-hook (lambda () (highlight-parentheses-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some minor config
(setq-default tab-width 2)
(display-time-mode 1)					;display clock at modeline
(line-number-mode 1)					; Show line number in the mode line.
(column-number-mode 1)					;Show column number in the mode line.
(global-linum-mode 1)					;show line number
(split-window-right)					;auto split window on starup
(setq-default cursor-type 'bar)			;set cursor to a thin vertical line instead of a little box
(global-hl-line-mode 1)					;highlight current line
(tool-bar-mode -1)						;turn off tool bar
(setq visible-bell t)					;make audible ring instead of bell
(setq inhibit-startup-message t)		;not display welcome message
(setq uniquify-buffer-name-style 'forward)
(setq whitespace-style '(face trailing lines-tail tabs))
(show-paren-mode 1)						;highlight matching paren
(set-default 'indicate-empty-lines t)
(setq split-width-threshold nil)        ;not allow split window horizontally
                                        ;automatically
(setq-default show-trailing-whitespace t)

;;; finally, provide the library
(provide 'tmtxt-appearance)
