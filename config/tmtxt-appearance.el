;;; how my emacs appears
;;; this file should be loaded after other files if want to override custom face

;;; load my custom theme
(setq custom-theme-directory "~/.emacs.d/lib/themes/")
(add-to-list 'custom-theme-load-path custom-theme-directory)
(load-theme 'tmtxt t)

;; Bigger minibuffer text
(defun tmtxt/minibuffer-setup ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 1.2)))
  (setq line-spacing 0.2))
(add-hook 'minibuffer-setup-hook 'tmtxt/minibuffer-setup)

;;; transparent emacs
(defun tmtxt/toggle-alpha ()
  (interactive)
  (let ((a (frame-parameter nil 'alpha)))
    (if (or (not (numberp a)) (= a 100))
        (set-frame-parameter nil 'alpha 88)
      (set-frame-parameter nil 'alpha 100))))

;; Show line number in the mode line.
(line-number-mode 1)

;; Show column number in the mode line.
(column-number-mode 1)

;;; show line number
(global-linum-mode 1)

;;; auto split window on starup
(split-window-right)

;;; set the font to support unicode
(tmtxt/in '(darwin)
  (set-frame-font  "Monaco-12"))

;;; set cursor to a thin vertical line instead of a little box
(setq-default cursor-type 'bar)

;;; hide the menu bar on OSX
(setq ns-auto-hide-menu-bar t)
(set-frame-position (selected-frame) 0 -50)

;;; idle highlight mode
(require 'idle-highlight-mode)
(remove-hook 'prog-mode-hook 'idle-highlight-mode)

;;; smooth scroll
;;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling    
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;; finally, provide the library
(provide 'tmtxt-appearance)
