;;; config for helm

;;; enable helm
(helm-mode 1)

(tmtxt/in '(darwin)
  (setq helm-locate-command "mdfind %s %s"))

(tmtxt/set-up 'helm-dired-recent-dirs)

;;; finally provide the library
(provide 'tmtxt-helm)
