;;; config for helm

(helm-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm locate
(setq helm-locate-command "mdfind %s %s")
(global-set-key (kbd "C-M-l") 'helm-locate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm swoop
;; (global-set-key (kbd "M-i") 'helm-swoop)
;; (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;; (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

;;; finally provide the library
(provide 'tmtxt-helm)
