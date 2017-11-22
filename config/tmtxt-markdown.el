;;; config for working with markdown mode

(tmtxt/add-lib "livedown")

(require 'markdown-mode)
(require 'ac-ispell)
(when (executable-find "livedown")
  (require 'livedown))

;;; associate
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd" . markdown-mode))

;;; config
(setq-default
 ac-ispell-requires 4
 )

;;; setup
(ac-ispell-setup)

;;; markdown mode hook
(add-hook 'markdown-mode-hook
          (lambda ()
            (flyspell-mode 1)
            (flyspell-buffer)
            (auto-complete-mode)
            (add-to-list 'ac-sources 'ac-source-ispell)
            (toggle-truncate-lines)
            ))

(provide 'tmtxt-markdown)
