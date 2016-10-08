;;; evil vim

(require 'evil)
(require 'key-chord)
(require 'evil-nerd-commenter)
(require 'evil-matchit)

;;; enable
(evil-mode 1)
(evilnc-default-hotkeys)
(global-evil-matchit-mode 1)

;;; not use evil insert state, prefer emacs state
(add-hook 'evil-insert-state-entry-hook
          (lambda ()
            (evil-exit-emacs-state)
            (evil-emacs-state 1)))


;;; default state for modes
;;; emacs state
(dolist (mode '(git-commit-mode
                comint-mode
                nodejs-repl-mode
                dired-mode
                wdired-mode
                skewer-repl-mode
                fundamental-mode
                inferior-moz-mode
                process-menu-mode
                cider-repl-mode
                neo4j-shell-mode
                sql-interactive-mode))
  (add-to-list 'evil-emacs-state-modes mode))

;;; motion state
(dolist (mode '())
  (add-to-list 'evil-motion-state-modes mode))

;;; normal state
(dolist (mode '(Info-mode))
  (add-to-list 'evil-normal-state-modes mode))


(defun tmtxt/repl-goto-prompt ()
  "Go to prompt when enter insert state for repl modes"
  (when (member major-mode
                '(eshell-mode
                  sql-interactive-mode
                  inferior-moz-mode
                  inferior-emacs-lisp-mode
                  inferior-lisp-mode
                  cider-repl-mode
                  skewer-repl-mode))
    (goto-char (point-max))))
(add-hook 'evil-emacs-state-entry-hook 'tmtxt/repl-goto-prompt)

;; key-chord
;; use jj to switch from insert state to normal or motion state
(key-chord-mode 1)

(setq-default
 key-chord-one-key-delay 0.5)

(defun tmtxt/evil-exit-insert-state ()
  "Exit evil insert state and change to normal or motion mode"
  (interactive)
  (cond                               ;decide which state to switch to
   ;; switch these mode to motion state
   ((member major-mode '(dired-mode)) (evil-motion-state 1))

   ;; do nothing for these mode
   ((member major-mode '()))

   ;; default switch to normal state
   (t (evil-normal-state 1))))

(key-chord-define evil-emacs-state-map "jj" 'tmtxt/evil-exit-insert-state)


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
      evil-normal-state-tag nil ;; (propertize "« N »" 'face 'tmtxt/evil-normal-tag)
      evil-motion-state-tag nil ;; (propertize "« M »" 'face 'tmtxt/evil-normal-tag)
      evil-insert-state-tag nil ;; (propertize "« I »" 'face 'tmtxt/evil-insert-tag)
      evil-emacs-state-tag nil ;; (propertize "« E »" 'face 'tmtxt/evil-emacs-tag)
      evil-visual-state-tag nil ;; (propertize "« ∞ »" 'face 'tmtxt/evil-visual-tag)
      evil-motion-state-cursor '(box "YellowGreen")
      evil-normal-state-cursor '(box "YellowGreen")
      evil-insert-state-cursor '(bar "White")
      evil-emacs-state-cursor '(bar "White")
      evil-visual-state-cursor '(box "#F86155")
      evil-default-state 'emacs)

(provide 'tmtxt-evil)
