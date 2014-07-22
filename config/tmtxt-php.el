;;; config for php coding

;;; some require packages
(defun tmtxt/setup-php ()
  (web-mode)
  (make-local-variable 'web-mode-code-indent-offset)
  (make-local-variable 'web-mode-markup-indent-offset)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-markup-indent-offset 2))

;;; auto enable php mode for .php and .inc files
(add-to-list 'auto-mode-alist '("\\.php$" . tmtxt/setup-php))
(add-to-list 'auto-mode-alist '("\\.inc$" . tmtxt/setup-php))

;;; finally provide the library
(provide 'tmtxt-php)
