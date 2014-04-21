(evil-mode 1)

;;; not use evil insert state, prefer emacs state
(add-hook 'evil-insert-state-entry-hook (lambda () (evil-emacs-state 1)))

;;; default state
(dolist (mode '(git-commit-mode))
  (add-to-list 'evil-insert-state-modes mode))

(dolist (mode '(dired-mode))
  (add-to-list 'evil-motion-state-modes mode))

(provide 'tmtxt-evil)
