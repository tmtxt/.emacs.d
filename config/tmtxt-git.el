;;; git
(require 'magit)
(require 'git-commit-mode)
(require 'gist)
(require 'git-messenger)

;;; commit length
(setq-default
 gist-view-gist t
 git-commit-summary-max-length 80
 magit-stage-all-confirm nil
 magit-use-overlays nil)

;;; hook
(add-hook 'git-commit-mode-hook
          (lambda () (flyspell-mode 0)) t)

(add-hook 'magit-mode-hook 'magit-load-config-extensions)

(add-hook 'git-messenger:after-popup-hook
          (lambda ()
            (evil-exit-emacs-state)))

(defun tmtxt/increase-tag ()
  "Auto increase tag for the last version number. 0.0.1 -> 0.0.2"
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


(defvar tmtxt-ar-connect-path "~/Projects/agencyrevolution/connect")

(defun tmtxt/ar-read-service ()
  "Read the service name"
  (let* ((connect-path tmtxt-ar-connect-path)
         (frontend-dirs (-> connect-path
                            (s-concat "/frontend")
                            (directory-files)))
         (nodejs-dirs (-> connect-path
                          (s-concat "/nodejs")
                          (directory-files)))
         (golang-dirs (-> connect-path
                          (s-concat "/golang")
                          (directory-files)))
         (all-dirs (-concat frontend-dirs nodejs-dirs golang-dirs)))
    (helm-comp-read "Select service to tag: " all-dirs)))

(defun tmtxt/ar-tag-less? (tag1 tag2)
  "Whether tag1 is less then tag2"
  (let* ((tag1 (->> tag1
                    (s-match "[0-9]+\.[0-9]+\.[0-9]+")
                    (-last-item)))
         (tag2 (->> tag2
                    (s-match "[0-9]+\.[0-9]+\.[0-9]+")
                    (-last-item))))
    (version< tag1 tag2)))

(defun tmtxt/ar-get-tags (service-name)
  "Get all the matching tags for this service name, sorted by tag order"
  (let* ((default-directory tmtxt-ar-connect-path)
         (all-tags (magit-git-lines "tag"))
         (match-tags (-filter (lambda (tag) (s-contains? service-name tag)) all-tags))
         (sorted-tags (-sort 'tmtxt/ar-tag-less? match-tags)))
    sorted-tags))

(defun tmtxt/ar-get-next-tag (tag)
  "Get the next tag, react.revolution-v0.2.3 will return react.revolution-v0.2.4"
  (let* ((regex "[0-9]+\.[0-9]+\.[0-9]+")
         (versions (->> tag
                        (s-match regex)
                        (-last-item))) ;major, minor, patch
         (tag-no-version (s-chop-suffix versions tag))
         (versions (s-split "[.]" versions))
         (minor-version (->> versions
                             (-last-item)
                             (string-to-number)))
         (next-minor-version (->> minor-version
                                  (+ 1)
                                  (number-to-string)))
         (next-versions `(,(nth 0 versions)
                          ,(nth 1 versions)
                          ,next-minor-version))
         (next-versions (s-join "." next-versions))
         (next-version (s-concat tag-no-version next-versions))
         )
    next-version
    ))

(defun tmtxt/ar-increase-tag ()
  "Prompt for a service and increase its tag"
  (interactive)
  (let* ((service-name (tmtxt/ar-read-service))
         (tags (tmtxt/ar-get-tags service-name))
         (latest-tag (-last-item tags))
         (next-tag (tmtxt/ar-get-next-tag latest-tag))
         (default-directory tmtxt-ar-connect-path))
    (magit-tag next-tag "HEAD")
    (message next-tag)
    next-tag
    ))

(provide 'tmtxt-git)
