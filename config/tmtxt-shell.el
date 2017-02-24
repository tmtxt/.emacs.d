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

(defun tmtxt/eshell ()
  (interactive)
  (let ((cd-eshell (lambda ()
                     (eshell/cd default-directory)
                     (eshell-reset))))
    (if (get-buffer "*eshell*")
        (switch-to-buffer "*eshell*")
      (call-interactively 'eshell))
    (unless (get-buffer-process (current-buffer))
      (funcall cd-eshell)))
  )

;;; hook
(add-hook 'eshell-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;;; env
;; (add-hook 'eshell-mode-hook 'exec-path-from-shell-initialize)


(provide 'tmtxt-shell)
;;; tmtxt-shell.el ends here
