;;; config for managing project

(require 'projectile)

;;; enable
(projectile-global-mode)

;;; config
(setq-default
 projectile-enable-caching t
 projectile-indexing-method 'hybrid)

(provide 'tmtxt-project)
