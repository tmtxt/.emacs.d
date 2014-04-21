;;; config for helm

;;; enable helm
(helm-mode 1)

;;; helm locate
(tmtxt/in '(darwin)
  (setq helm-locate-command "mdfind %s %s"))

;;; helm recent dir
(tmtxt/set-up 'helm-dired-recent-dirs)

;;; helm desc bind
(tmtxt/set-up 'helm-descbinds
  (helm-descbinds-install))

;;; some variables
(setq
 helm-mp-highlight-delay 0.7
 helm-mp-highlight-threshold 4
 helm-maybe-use-default-as-input t
 helm-quick-update t
 helm-idle-delay 0.01
 helm-input-idle-delay 0.01
 helm-always-two-windows nil
 helm-split-window-default-side 'other
 helm-candidate-number-limit 200
 )

;;; finally provide the library
(provide 'tmtxt-helm)
