;;; config for org mode

(require 'org)

;;; some recomended settings from org mode manual
(add-hook 'org-mode-hook (lambda ()
                           (flyspell-mode 1)
                           (org-indent-mode 1)
                           (turn-on-font-lock)))

(setq-default
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
 org-special-ctrl-k t

 ;; speed keys
 org-use-speed-commands t
 )

(add-to-list 'org-speed-commands-user
             '("k" org-speed-move-safe 'outline-next-visible-heading))
(add-to-list 'org-speed-commands-user
             '("i" org-speed-move-safe 'outline-previous-visible-heading))
(add-to-list 'org-speed-commands-user
             '("l" org-speed-move-safe 'org-forward-heading-same-level))
(add-to-list 'org-speed-commands-user
             '("j" org-speed-move-safe 'org-backward-heading-same-level))

;;; finally, provide the library
(provide 'tmtxt-org)
