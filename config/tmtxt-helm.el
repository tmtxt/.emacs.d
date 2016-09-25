;;; config for helm

(require 'helm)
(require 'helm-descbinds)

;;; enable helm
(helm-mode 1)
(helm-descbinds-install)

;;; some variables
(setq-default
 helm-mp-highlight-delay 0.7
 helm-mp-highlight-threshold 4
 helm-maybe-use-default-as-input nil
 helm-quick-update t
 helm-idle-delay 0.01
 helm-input-idle-delay 0.01
 helm-always-two-windows nil
 helm-split-window-default-side 'other
 helm-candidate-number-limit 200

 helm-locate-command (case system-type
                       ('darwin "mdfind %s %s")
                       ('gnu/linux "locate %s -r %s"))
 )

;;; custom helm source for quick jump
(defun tmtxt/helm-sources ()
  (let ((base '(helm-source-projectile-projects
                helm-source-buffers-list
                helm-source-files-in-current-dir
                helm-source-bookmarks
                helm-source-recentf
                helm-source-projectile-buffers-list
                helm-source-projectile-files-list
                helm-source-file-cache)))
    base

    ;; (if (featurep 'helm-cmd-t)
    ;;     ;; FIX
    ;;     (condition-case nil
    ;;         (cons (helm-cmd-t-get-create-source (helm-cmd-t-root-data)) base)
    ;;       (error base))
    ;;   base)
    ))

(defun tmtxt/helm ()
  (interactive)
  (helm-other-buffer (tmtxt/helm-sources) "*tmtxt/helm*"))

;;; auto turn on helm follow mode
(add-hook 'helm-before-initialize-hook
          #'(lambda ()
              (dolist (source (list helm-source-occur))
                (when source
                  (helm-attrset 'follow 1 source)))))

;;; helm always display at the bottom
(add-to-list 'display-buffer-alist
                    `(,(rx bos "*helm" (* not-newline) "*" eos)
                         (display-buffer-in-side-window)
                         (inhibit-same-window . t)
                         (window-height . 0.4)))

;;; finally provide the library
(provide 'tmtxt-helm)
