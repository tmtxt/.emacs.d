;;; grep

(require 'grep)

;;; ignore from grep
(add-to-list 'grep-find-ignored-files "*.class")
(add-to-list 'grep-find-ignored-directories "node_modules")
(add-to-list 'grep-find-ignored-directories "dist")

(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))
(add-hook 'helm-grep-mode-hook (lambda () (toggle-truncate-lines 1)))

(provide 'tmtxt-grep)
