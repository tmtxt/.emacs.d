;;; commit length
(tmtxt/set-up 'git-commit-mode
  (setq git-commit-summary-max-length 70))

(add-hook 'git-commit-mode-hook
          (lambda () (flyspell-mode 0)) t)

(add-hook 'magit-mode-hook 'magit-load-config-extensions)
(set-default 'magit-stage-all-confirm nil)
(setq magit-use-overlays nil)

(tmtxt/set-up 'gist
  (setq gist-view-gist t))

(provide 'tmtxt-git)
