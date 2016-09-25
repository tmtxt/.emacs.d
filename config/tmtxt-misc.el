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
