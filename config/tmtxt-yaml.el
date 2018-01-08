(require 'yaml-mode)

(add-hook 'yaml-mode-hook 'auto-complete-mode)
(add-hook 'yaml-mode-hook 'toggle-truncate-lines)

(provide 'tmtxt-yaml)
