;;; remove trailing white space since python uses indentation for code block
(defun tmtxt/setup-python ()
  (add-hook 'before-save-hook 'tmtxt/edit-before-save-prog nil t))
(add-hook 'python-mode-hook 'tmtxt/setup-python)

;;; syntax checking using pyflakes
(require 'flycheck-pyflakes)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-to-list 'flycheck-disabled-checkers 'python-flake8)
(add-to-list 'flycheck-disabled-checkers 'python-pylint)

(provide 'tmtxt-python)
