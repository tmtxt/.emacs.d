;;; config for helm

(require 'helm)
(require 'helm-descbinds)
(require 'helm-cmd-t)
(require 'helm-bookmark)

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


(defun tmtxt/helm-sources ()
  "My combined sources"
  (let ((base '(helm-source-projectile-projects
                helm-source-buffers-list
                helm-source-bookmarks
                ;; helm-source-recentf
                ;; helm-source-files-in-current-dir
                ;; helm-source-projectile-buffers-list
                ;; helm-source-projectile-files-list
                ;; helm-source-file-cache
                )))
    base))

(defun tmtxt/helm ()
  (interactive)
  (helm-other-buffer (tmtxt/helm-sources) "*helm/tmtxt*"))

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

;;; set executable path for ag on Windows
;;; install ag in this path
;;; download ag from https://github.com/k-takata/the_silver_searcher-win32
(tmtxt/in '(windows-nt)
  (setenv "PATH" (concat (getenv "PATH") "C:\\Program Files\\ag"))
  (setq exec-path (append exec-path '("C:\\Program Files\\ag")))
)

;;; finally provide the library
(provide 'tmtxt-helm)
