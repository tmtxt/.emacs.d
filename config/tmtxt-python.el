;;; remove trailing white space since python uses indentation for code block
(add-hook 'python-mode-hook 'tmtxt/prog-mode-setup)

(provide 'tmtxt-python)
