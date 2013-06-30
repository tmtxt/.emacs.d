;;; config for php coding

;;; some require packages
(require 'php-mode)

;;; auto enable php mode for .php and .inc files
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;;; finally provide the library
(provide 'tmtxt-php)
