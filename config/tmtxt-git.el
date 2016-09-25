(require 'magit)

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

(defun tmtxt/increase-tag () "Auto increase tag"
       (interactive)
       (let* ((current-version (->> (magit-get-current-tag)
                                    (s-split "[.]")))
              (minor-version (-> current-version
                                 (-last-item)
                                 (string-to-number)))
              (next-minor-version (-> minor-version (+ 1) (number-to-string)))
              (last-pos (- (length current-version) 1))
              (next-version (-replace-at last-pos next-minor-version current-version))
              (next-version (s-join "." next-version)))
         (magit-tag next-version "HEAD")
         (message next-version)))

(set-face-foreground 'magit-diff-add "green4")
(set-face-foreground 'magit-diff-del "red3")

(tmtxt/set-up 'git-messenger
  (add-hook 'git-messenger:after-popup-hook
            (lambda ()
              (evil-exit-emacs-state))))

(provide 'tmtxt-git)
