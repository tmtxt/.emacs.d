;;; turn of paredit for non lisp mode
(defun tmtxt-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

;;; add watch words
(defun tmtxt-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'tmtxt-add-watchwords)

;;; auto fill comment
(defun tmtxt-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))
(add-hook 'prog-mode-hook 'tmtxt-local-comment-auto-fill)

;;; rest client
(require 'restclient)
(setq auto-mode-alist
      (cons '("\\.rest" . restclient-mode) auto-mode-alist))
(add-hook 'restclient-mode-hook 'auto-complete-mode)

;;; other emacs starter kit config
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq oddmuse-directory (concat user-emacs-directory "oddmuse"))
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq diff-switches "-u")
(set-default 'imenu-auto-rescan t)
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'tmtxt-misc)
