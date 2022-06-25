;;; git
(require 'magit)

;; commit length
(setq-default
 git-commit-summary-max-length 100
 magit-no-confirm '(stage-all-changes))

(provide 'tmtxt-git)
