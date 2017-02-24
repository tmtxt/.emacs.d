;;; shell --- config for using shell in emacs

;;; Commentary:
;;; Author: truongtx

(require 'eshell)
(require 'exec-path-from-shell)

;;; Code:

;;; copy PATH from my default shell
(add-to-list 'exec-path-from-shell-variables "NODE_PATH")
(exec-path-from-shell-initialize)

;;; disable trailing whitespace for term modes
(dolist (hook '(eshell-mode-hook
                term-mode-hook))
  (add-hook hook
            (lambda ()
              (setq-local show-trailing-whitespace nil))))

;;; the rest is eshell config
;;; eshell
(setq-default
 eshell-save-history-on-exit t          ;save history
 eshell-cmpl-cycle-completions t        ;TAB for suggestion
 eshell-buffer-shorthand t              ;shorthand buffer name
 )


(defun tmtxt/eshell (&optional arg)
  "Wrapper around default eshell command to keep the exec path and other env as expected"
  (interactive "P")
  (let ((buf (call-interactively 'eshell arg))
        (cd-eshell (lambda ()
                     (eshell/cd default-directory)
                     (eshell-reset))))
    (unless (get-buffer-process buf)
      (funcall cd-eshell))))

;;; hook
(add-hook 'eshell-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(provide 'tmtxt-shell)
;;; tmtxt-shell.el ends here
