;;; remove trailing white space since python uses indentation for code block
(defun tmtxt/setup-python ()
  (add-hook 'before-save-hook 'tmtxt/edit-before-save-prog nil t))
(add-hook 'python-mode-hook 'tmtxt/setup-python)

;;; syntax checking using pyflakes
(require 'flycheck-pyflakes)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-to-list 'flycheck-disabled-checkers 'python-flake8)
(add-to-list 'flycheck-disabled-checkers 'python-pylint)

;;; elpy
(require 'elpy)
(setq elpy-modules
      (delete 'elpy-module-flymake elpy-modules))
(add-hook 'elpy-mode-hook (tmtxt/on-fn 'flycheck-mode))
(setq elpy-rpc-backend "jedi")
(elpy-enable)
(elpy-use-ipython)

(provide 'tmtxt-python)
