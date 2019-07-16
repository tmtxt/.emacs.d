(require 'yaml-mode)

(add-hook 'yaml-mode-hook 'auto-complete-mode)
(add-hook 'yaml-mode-hook 'toggle-truncate-lines)
(add-hook 'yaml-mode-hook 'tmtxt/prog-mode-setup)
(add-hook 'yaml-mode-hook (lambda () (setq fill-column 700)))

(provide 'tmtxt-yaml)
