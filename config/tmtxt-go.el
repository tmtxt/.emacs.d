;;; Config for golang

(require 'go-mode)

;;; auto format go code when save
(add-hook 'before-save-hook #'gofmt-before-save)

(provide 'tmtxt-go)
