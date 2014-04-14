;;; config for org mode

(require 'org)
(require 'tmtxt-util)

;;; some recomended settings from org mode manual
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(add-hook 'org-mode-hook (tmtxt/on-fn 'org-indent-mode))

(setq
 ;; fontify code block
 org-src-fontify-natively t

 ;; not allow to edit in the foded region
 org-catch-invisible-edits 'error

 ;; subscript and superscript
 org-use-sub-superscripts '{}
 org-export-with-sub-superscripts '{}

 ;; M-Return (insert new heading at same level)
 org-M-RET-may-split-line nil           ;don't split line
 org-insert-heading-respect-content t   ;insert after the content

 ;; indentation
 org-indent-indentation-per-level 1
 org-list-indent-offset 1

 ;; org goto
 org-goto-auto-isearch nil
 org-goto-interface 'outline-path-completion

 ;; special C-a, C-e, C-k
 org-special-ctrl-a/e t
 org-special-ctrl-k t)

(dolist (hook '(org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;;; finally, provide the library
(provide 'tmtxt-org)
