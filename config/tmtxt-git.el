;;; commit length
(tmtxt/set-up 'git-commit-mode
  (setq git-commit-summary-max-length 70))

(add-hook 'git-commit-mode-hook
          (lambda () (flyspell-mode 0)) t)

(provide 'tmtxt-git)
