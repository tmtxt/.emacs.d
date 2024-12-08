;;; tmtxt-cc --- Config for C/C++ coding
;;; This file should be loaded after tmtxt-editing.el for auto-complete-clang to work properly
(require 'cc-mode)

;;; some minor config
(setq-default
 c-basic-offset 4
 c-default-style "linux")

(provide 'tmtxt-cc)
