;;; config for helm

;;; enable helm
(helm-mode 1)

;;; helm locate
(tmtxt/in '(darwin)
  (setq helm-locate-command "mdfind %s %s"))
(tmtxt/in '(gnu/linux)
  (setq helm-locate-command "locate %s -r %s"))

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

;;; custom helm source for quick jump
(defun tmtxt/helm-sources ()
  (let ((base '(helm-source-buffers-list
                helm-source-ido-virtual-buffers
                helm-source-files-in-current-dir
                helm-source-pp-bookmarks
                helm-source-recentf
                helm-source-file-cache)))

    ;; locate source
    (tmtxt/in '(darwin)
      (add-to-list 'base 'helm-source-mac-spotlight))
    (tmtxt/in '(gnu/linux)
      (add-to-list 'base 'helm-source-locate))
    
    (if (featurep 'helm-cmd-t)
        ;; FIX
        (condition-case nil
            (cons (helm-cmd-t-get-create-source (helm-cmd-t-root-data)) base)
          (error base))
      base)))

(defun tmtxt/helm ()
  (interactive)
  (helm-other-buffer (tmtxt/helm-sources) "*tmtxt/helm*"))

;;; auto turn on helm follow mode
(add-hook 'helm-before-initialize-hook
          #'(lambda ()
              (dolist (source (list helm-source-flycheck))
                (helm-attrset 'follow 1 source))))

;;; finally provide the library
(provide 'tmtxt-helm)
