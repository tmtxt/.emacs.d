(defun tmtxt/setup-python ()
  (add-hook 'before-save-hook 'tmtxt/edit-before-save-prog nil t))

(add-hook 'python-mode-hook 'tmtxt/setup-python)

(provide 'tmtxt-python)
