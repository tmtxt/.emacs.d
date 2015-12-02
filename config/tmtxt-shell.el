;;; shell --- config for using shell in emacs

;;; Commentary:
;;; Author: truongtx

;;; Code:
;;; copy PATH from my default shell
(tmtxt/set-up 'exec-path-from-shell
  (exec-path-from-shell-initialize))

;;; eshell
(setq eshell-save-history-on-exit t)	;save history
(setq eshell-cmpl-cycle-completions t)	;TAB for suggestion
(setq eshell-buffer-shorthand t)		;shorthand buffer name

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local show-trailing-whitespace nil)))

(provide 'tmtxt-shell)
;;; tmtxt-shell.el ends here
