(require 'yaml-mode)

(add-hook 'yaml-mode-hook 'auto-complete-mode)
(add-hook 'yaml-mode-hook 'toggle-truncate-lines)
(add-hook 'yaml-mode-hook 'tmtxt/prog-mode-setup)

(provide 'tmtxt-yaml)
