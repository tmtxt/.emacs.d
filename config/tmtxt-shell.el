;;; config for working with shell command in emacs

;;; copy PATH from my default shell
(exec-path-from-shell-initialize)

;;; eshell
(setq eshell-save-history-on-exit t)	;save history
(setq eshell-cmpl-cycle-completions t)	;TAB for suggestion
(setq eshell-buffer-shorthand t)		;shorthand buffer name

;;; key binding for eshell
(global-set-key (kbd "C-x m") 'eshell)
;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x C-m") (lambda () (interactive) (eshell t)))

(provide 'tmtxt-shell)
