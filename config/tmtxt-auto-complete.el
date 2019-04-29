;;; tmtxt-auto-complete
;;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)

(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

;;; config
(setq-default
 ac-ignore-case nil
 ac-use-quick-help t
 ac-quick-help-delay 0.3)

;;; trigger key
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;;; I need completion in these faces, too
(delete 'font-lock-string-face ac-disable-faces)
(delete 'font-lock-comment-face ac-disable-faces)
(delete 'font-lock-doc-face ac-disable-faces)

(provide 'tmtxt-auto-complete)