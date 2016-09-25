;;; shell --- config for using shell in emacs

;;; Commentary:
;;; Author: truongtx

(require 'eshell)
(require 'exec-path-from-shell)

;;; Code:

;;; copy PATH from my default shell
(exec-path-from-shell-initialize)

;;; eshell
(setq-default
 eshell-save-history-on-exit t          ;save history
 eshell-cmpl-cycle-completions t        ;TAB for suggestion
 eshell-buffer-shorthand t              ;shorthand buffer name
 )

;;; disable trailing whitespace for term modes
(dolist (hook '(eshell-mode-hook
                term-mode-hook))
  (add-hook hook
            (lambda ()
              (setq-local show-trailing-whitespace nil))))

(provide 'tmtxt-shell)
;;; tmtxt-shell.el ends here
