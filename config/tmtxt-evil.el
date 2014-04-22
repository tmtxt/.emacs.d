(evil-mode 1)

;;; not use evil insert state, prefer emacs state
(add-hook 'evil-insert-state-entry-hook (lambda () (evil-emacs-state 1)))

;;; default state
(dolist (mode '(git-commit-mode
                dired-mode
                twittering-mode))
  (add-to-list 'evil-insert-state-modes mode))

(dolist (mode '())
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

;;; evil indicator
(defface tmtxt/evil-insert-tag
  `((t (:inherit font-lock-comment-delimiter-face :slant normal :weight bold)))
  "Evil insert mode indicator face")
(defface tmtxt/evil-normal-tag
  `((t (:inherit diredp-mode-line-flagged :weight bold :foreground "#C88BA7")))
  "Evil normal mode indicator face")
(defface tmtxt/evil-emacs-tag
  `((t (:inherit diredp-mode-line-marked :weight bold :foreground "#2E9B98")))
  "Evil emacs mode indicator face")
(defface tmtxt/evil-visual-tag
  `((t (:inherit font-lock-preprocessor-face)))
  "Evil visual mode indicator face")

(setq evil-mode-line-format 'before
      evil-normal-state-tag (propertize "« N »" 'face 'tmtxt/evil-normal-tag)
      evil-motion-state-tag (propertize "« M »" 'face 'tmtxt/evil-normal-tag)
      evil-insert-state-tag (propertize "« I »" 'face 'tmtxt/evil-insert-tag)
      evil-emacs-state-tag (propertize "« E »" 'face 'tmtxt/evil-emacs-tag)
      evil-visual-state-tag (propertize "« ∞ »" 'face 'tmtxt/evil-visual-tag)
      evil-motion-state-cursor '(box "YellowGreen")
      evil-normal-state-cursor '(box "YellowGreen")
      evil-insert-state-cursor '(bar "YellowGreen")
      evil-emacs-state-cursor '(bar "YellowGreen")
      evil-visual-state-cursor '(box "#F86155"))

(provide 'tmtxt-evil)
