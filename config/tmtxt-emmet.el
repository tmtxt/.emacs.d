;;; config for emmet mode

;;; required library
(require 'tmtxt-util)

;;; add to load path
(tmtxt/add-lib "emmet-mode")

;;; require the package
(require 'emmet-mode)

;;; auto start on sgml and css mode
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;;; default indentation for html mode
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;indent 2 spaces.

;;; provide
(provide 'tmtxt-emmet)
