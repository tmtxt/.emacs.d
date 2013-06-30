;;; config for textmate mode

;;; include textmate
(require 'textmate)

;;; shift text left and shift text right key bindings
(global-set-key (kbd "C-S-j") 'textmate-shift-left)
(global-set-key (kbd "C-S-l") 'textmate-shift-right)

;;; finally provide the library
(provide 'tmtxt-textmate)
