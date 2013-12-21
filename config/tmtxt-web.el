;;; config for working with web page (html, xml,...)

;;; required library
(require 'tmtxt-util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emmet mode
(require 'emmet-mode)

;;; auto start on sgml and css mode
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;;; default indentation for html mode
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;indent 2 spaces.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web mode
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;; disable rainbow-mode and whitespace-mode when use web-mode
(defun fix-web-mode-font ()
  "Disable font-lock-mode, rainbow-mode and whitespace-mode"
  (rainbow-mode 0)
  (whitespace-mode 0)
  (font-lock-mode 0)
  (set-face-attribute 'web-mode-doctype-face nil :foreground "#CB4B16" :bold t)
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "#859900" :bold t)
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "DodgerBlue3"))
(add-hook 'web-mode-hook 'fix-web-mode-font)

;;; indentation
(setq web-mode-markup-indent-offset 2) ;html indentation
(setq web-mode-css-indent-offset 2)	;css indentation
(setq web-mode-code-indent-offset 2)	;script
(setq web-mode-indent-style 2)		;fix side effect for html indentation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc
;;; auto insert and tag when typing </
(setq nxml-slash-auto-complete-flag t)

(provide 'tmtxt-web)
