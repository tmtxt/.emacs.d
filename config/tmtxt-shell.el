;;; config for working with shell command in emacs

;;; copy PATH from my default shell
(tmtxt/set-up 'exec-path-from-shell
  (exec-path-from-shell-initialize))

;;; eshell
(setq eshell-save-history-on-exit t)	;save history
(setq eshell-cmpl-cycle-completions t)	;TAB for suggestion
(setq eshell-buffer-shorthand t)		;shorthand buffer name

(provide 'tmtxt-shell)
