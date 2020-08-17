;;; git
(require 'magit)

;; commit length
(setq-default
 git-commit-summary-max-length 100)

;;; hook
;; (add-hook 'git-commit-mode-hook
;;           (lambda ()
;;             (flyspell-mode 0)
;;             (setq fill-column 100)
;;             ) t)

;; (add-hook 'magit-mode-hook 'magit-load-config-extensions)

;; (add-hook 'git-messenger:after-popup-hook
;;           (lambda ()
;;             (evil-exit-emacs-state)))

;; (defun tmtxt/increase-tag ()
;;   "Auto increase tag for the last version number. 0.0.1 -> 0.0.2"
;;   (interactive)
;;   (let* ((current-version (->> (magit-get-current-tag)
;;                                (s-split "[.]")))
;;          (minor-version (-> current-version
;;                             (-last-item)
;;                             (string-to-number)))
;;          (next-minor-version (-> minor-version (+ 1) (number-to-string)))
;;          (last-pos (- (length current-version) 1))
;;          (next-version (-replace-at last-pos next-minor-version current-version))
;;          (next-version (s-join "." next-version)))
;;     (magit-tag next-version "HEAD")
;;     (message next-version)))

;; (set-face-foreground 'magit-diff-add "green4")
;; (set-face-foreground 'magit-diff-del "red3")

(provide 'tmtxt-git)
