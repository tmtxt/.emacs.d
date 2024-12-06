;;; some of the code is from emacs starter kit

(require 'flycheck)

;;; LISP & EMACS LISP
;;; hook
;; (add-hook 'lisp-mode-hook 'enable-paredit-mode)
(dolist (f '(;; enable-paredit-mode
             turn-on-eldoc-mode
             elisp-slime-nav-mode
             tmtxt/remove-elc-on-save
             tmtxt/add-elisp-font-lock
             tmtxt/prog-mode-setup))
  (add-hook 'emacs-lisp-mode-hook f))

(defun tmtxt/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(defun tmtxt/add-elisp-font-lock ()
  "Add font lock keywords for emacs lisp"
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(provide 'tmtxt-lisp)
