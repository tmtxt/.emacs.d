;;; config for org mode

(require 'org)
(require 'tmtxt-util)

;;; some recomended settings from org mode manual
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(add-hook 'org-mode-hook (tmtxt/on-fn 'org-indent-mode))

(setq
 org-src-fontify-natively t)

;;; finally, provide the library
(provide 'tmtxt-org)
