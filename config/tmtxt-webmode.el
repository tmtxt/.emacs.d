;;; config file for web-mode

;;; include the library
(require 'web-mode)

;;; associate file types
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;; disable rainbow-mode and whitespace-mode when use web-mode
(defun disable-rainbow-whitespace-mode ()
  "Disable rainbow-mode and whitespace-mode"

  (whitespace-mode 0))
(add-hook 'web-mode-hook 'disable-rainbow-whitespace-mode)


  (whitespace-mode 0)

;;; indentation
(setq web-mode-markup-indent-offset 2) ;html indentation
(setq web-mode-css-indent-offset 2)	;css indentation
(setq web-mode-code-indent-offset 2)	;script
(setq web-mode-indent-style 2)		;fix side effect for html indentation


(set-face-attribute 'web-mode-doctype-face nil :foreground "#CB4B16" :bold t)
(set-face-attribute 'web-mode-html-tag-face nil :foreground "#859900" :bold t)
(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "DodgerBlue3")  


;;; provide the library
(provide 'tmtxt-webmode)
