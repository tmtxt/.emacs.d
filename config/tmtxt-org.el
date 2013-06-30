;;; config for org mode

(require 'org)
(require 'tmtxt-util)

;;; some recomended settings from org mode manual
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;; rebinding keys
(global-unset-key (kbd "C-M-j"))
(global-unset-key (kbd "C-M-l"))

;;; move meta up/down/left/right
(define-key org-mode-map (kbd "C-s-j") 'org-metaleft)
(define-key org-mode-map (kbd "C-s-l") 'org-metaright)
(define-key org-mode-map (kbd "C-s-i") 'org-metaup)
(define-key org-mode-map (kbd "C-s-k") 'org-metadown)
;;; for OSX
(tmtxt/in '(darwin)
  (define-key org-mode-map (kbd "<C-s-268632074>") 'org-metaleft)
  (define-key org-mode-map (kbd "<C-s-268632076>") 'org-metaright)
  (define-key org-mode-map (kbd "<C-s-268632073>") 'org-metaup)
  (define-key org-mode-map (kbd "<C-s-268632075>") 'org-metadown))

;;; shift meta up/down/left/right
;;; promote/demote current subtree
(define-key org-mode-map (kbd "C-S-s-j") 'org-shiftmetaleft)
(define-key org-mode-map (kbd "C-S-s-l") 'org-shiftmetaright)
;;; move tree up/down
(define-key org-mode-map (kbd "C-S-s-i") 'org-shiftmetaup)
(define-key org-mode-map (kbd "C-S-s-k") 'org-shiftmetadown)

;;; finally, provide the library
(provide 'tmtxt-org)
