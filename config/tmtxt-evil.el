(evil-mode 1)

;;; not use evil insert state, prefer emacs state
(add-hook 'evil-insert-state-entry-hook (lambda () (evil-emacs-state 1)))

;;; default state
(dolist (mode '(git-commit-mode))
  (add-to-list 'evil-insert-state-modes mode))

(dolist (mode '(dired-mode))
  (add-to-list 'evil-motion-state-modes mode))

;;; use jj to switch from insert state to normal or motion state 
(tmtxt/set-up 'key-chord
  (key-chord-mode 1)
  (defun tmtxt/evil-exit-insert-state ()
    "Exit evil insert state and change to normal or motion mode"
    (interactive)
    (if (member major-mode '(dired-mode))
        (evil-motion-state 1)
      (evil-normal-state 1)))
  (key-chord-define evil-emacs-state-map "jj" 'tmtxt/evil-exit-insert-state))

;;; go to promt when enter insert state for repl mode
(defun tmtxt/repl-goto-prompt ()
  (when (member major-mode
                '(eshell-mode
                  sql-interactive-mode
                  inferior-moz-mode
                  inferior-emacs-lisp-mode
                  inferior-lisp-mode
                  cider-repl-mode))
    (goto-char (point-max))))
(add-hook 'evil-emacs-state-entry-hook 'tmtxt/repl-goto-prompt)

(provide 'tmtxt-evil)
