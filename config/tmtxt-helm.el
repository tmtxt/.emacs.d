;;; config for helm

(require 'helm)
(require 'helm-descbinds)
(require 'helm-bookmark)
(require 'helm-projectile)

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
 helm-candidate-number-limit 200
 )
(tmtxt/in '(darwin)
  (setq-default
   helm-locate-command "mdfind %s %s"))
(tmtxt/in '(gnu/linux)
  (setq-default
   helm-locate-command "locate %s -r %s"))

(defun tmtxt/helm-sources ()
  "My combined sources"
  (let ((base '(helm-source-projectile-projects
                helm-source-buffers-list
                helm-source-bookmarks
                )))
    base))

(defun tmtxt/helm ()
  (interactive)
  (helm-other-buffer (tmtxt/helm-sources) "*helm/tmtxt*"))

;;; set executable path for ag on Windows
;;; install ag in this path
;;; download ag from https://github.com/k-takata/the_silver_searcher-win32
(tmtxt/in '(windows-nt)
  (setenv "PATH" (concat (getenv "PATH") "C:\\Program Files\\ag"))
  (setq exec-path (append exec-path '("C:\\Program Files\\ag")))
)

;;; finally provide the library
(provide 'tmtxt-helm)
