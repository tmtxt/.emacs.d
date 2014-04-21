(evil-mode 1)

;;; not use evil insert state, prefer emacs state
(add-hook 'evil-insert-state-entry-hook (lambda () (evil-emacs-state 1)))



(provide 'tmtxt-evil)
