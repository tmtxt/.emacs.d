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

;;; remove white spaces before saving
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; set fill column (auto new line when reach 80 character)
(setq-default fill-column 80)

;;; show line number
(global-linum-mode 1)

;;; auto split window on starup
(split-window-right)

;;; set the font to support unicode
(tmtxt/in '(darwin)
  (set-frame-font  "Monaco-12"))

;;; set cursor to a thin vertical line instead of a little box
(setq-default cursor-type 'bar)

;;; finally, provide the library
(provide 'tmtxt-appearance)
