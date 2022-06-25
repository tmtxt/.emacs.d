;;; config for working with markdown mode

(tmtxt/add-lib "livedown")

(require 'markdown-mode)
(when (executable-find "livedown")
  (require 'livedown))

;;; associate
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd" . markdown-mode))

;;; markdown mode hook
(add-hook 'markdown-mode-hook
          (lambda ()
            (auto-complete-mode)
            (toggle-truncate-lines)
            ))

(provide 'tmtxt-markdown)
