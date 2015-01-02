;;; config for working with markdown mode

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
;;; associate markdown with .md file
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.mkd" . markdown-mode) auto-mode-alist))

;; (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))

(when (executable-find "livedown")
  (progn
    (tmtxt/add-lib "livedown")
    (require 'livedown)))

(provide 'tmtxt-markdown)
