;;; shell --- config for using shell in emacs

;;; Commentary:
;;; Author: truongtx

(require 's)
(require 'eshell)
(require 'em-smart)
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
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(setq-default
 eshell-save-history-on-exit t          ;save history
 eshell-cmpl-cycle-completions t        ;TAB for suggestion
 eshell-buffer-shorthand t              ;shorthand buffer name
 )

;;; custom functions to activate eshell
(defun tmtxt/eshell (&optional arg)
  "Wrapper around default eshell command to keep the exec path and other env as expected"
  (interactive "P")
  (let ((buf (call-interactively 'eshell arg))
        (cd-eshell (lambda ()
                     (eshell/cd default-directory)
                     (eshell-reset))))
    (unless (get-buffer-process buf)
      (funcall cd-eshell))))

(defun tmtxt/eshell-change-buffer-name ()
  "Change the current eshell buffer name to current directory related name"
  (let* ((dir-name (-> default-directory
                       (directory-file-name)
                       (file-name-nondirectory)))
         (new-buffer-name (s-concat "*eshell " dir-name "*")))
    (rename-buffer new-buffer-name t)))

;;; hook
(add-hook 'eshell-mode-hook
          (lambda ()
            (toggle-truncate-lines t)
            (setq-local pcomplete-ignore-case t)))
(add-hook 'eshell-mode-hook 'tmtxt/eshell-change-buffer-name)

(add-hook 'eshell-directory-change-hook 'tmtxt/eshell-change-buffer-name)

(provide 'tmtxt-shell)
;;; tmtxt-shell.el ends here
