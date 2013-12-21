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

;;; bind key
(define-key emmet-mode-keymap (kbd "C-j") 'emmet-expand-yas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web mode
;;; associate with web mode
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))

;;; disable rainbow-mode and whitespace-mode when use web-mode
(defun web-mode-hook ()
  "Config for working with web mode"

  ;; disable rainbow, whitespace, idle highlight, font lock mode
  (rainbow-mode 0)
  (whitespace-mode 0)
  (font-lock-mode 0)
  (idle-highlight-mode 0)

  ;; indentation
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)

  ;; key bindings
  (define-key web-mode-map (kbd "<C-s-268632077>") 'web-mode-tag-match)
  (define-key web-mode-map (kbd "C-s-m") 'web-mode-tag-match)
  (define-key web-mode-map (kbd "<C-s-268632076>") 'web-mode-element-next)
  (define-key web-mode-map (kbd "C-s-l") 'web-mode-element-next)
  (define-key web-mode-map (kbd "<C-s-268632074>") 'web-mode-element-previous)
  (define-key web-mode-map (kbd "C-s-j") 'web-mode-element-previous)
  (define-key web-mode-map (kbd "<C-s-268632075>") 'web-mode-element-child)
  (define-key web-mode-map (kbd "C-s-k") 'web-mode-element-child)
  (define-key web-mode-map (kbd "<C-s-268632073>") 'web-mode-element-parent)
  (define-key web-mode-map (kbd "C-s-k") 'web-mode-element-parent)
  (define-key web-mode-map (kbd "<C-s-268632085>") 'web-mode-element-select)
  (define-key web-mode-map (kbd "C-s-u") 'web-mode-element-select)
  (define-key web-mode-map (kbd "<C-s-268632079>") 'web-mode-element-content-select)
  (define-key web-mode-map (kbd "C-s-o") 'web-mode-element-content-select)
  (define-key web-mode-map (kbd "<C-s-268632078>") 'web-mode-element-kill)
  (define-key web-mode-map (kbd "C-s-n") 'web-mode-element-kill)
  (define-key web-mode-map (kbd "<C-s-268632072>") 'web-mode-element-rename)
  (define-key web-mode-map (kbd "C-s-h") 'web-mode-element-rename)

  ;; set faces
  (set-face-attribute 'web-mode-doctype-face nil :foreground "#CB4B16" :bold t)
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "#859900" :bold t)
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "DodgerBlue3"))
(add-hook 'web-mode-hook 'web-mode-hook)

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
