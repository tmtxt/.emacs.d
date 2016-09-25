;;; config for working with web page (html, xml,...)

;;; required library
(require 'tmtxt-util)

;;; emmet mode
(require 'emmet-mode)
(require 'scss-mode)

;;; auto start on sgml and css mode
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook 'emmet-mode)

;;; default indentation for html mode
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;indent 2 spaces.

;;; bind key
(define-key emmet-mode-keymap (kbd "C-j") 'emmet-expand-yas)


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
(add-to-list 'auto-mode-alist '("\\.ejs?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mako$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mak$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;;; disable rainbow-mode and whitespace-mode when use web-mode
(defun web-mode-hook ()
  "Config for working with web mode"

  (tmtxt/prog-mode-setup)

  ;; disable rainbow, whitespace, idle highlight, font lock mode
  (rainbow-mode 0)
  (whitespace-mode 0)
  (font-lock-mode 1)
  (idle-highlight-mode 0)
  (setq fill-column 500)

  ;; indentation
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)

  ;; set faces
  (set-face-attribute 'web-mode-doctype-face nil :foreground "#CB4B16" :bold t)
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "#859900" :bold t)
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "DodgerBlue3")

  ;; highlight
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)

  ;; auto complete and tern
  (auto-complete-mode 1)
  (if web-mode-ac-sources-alist
      (progn
        (add-to-list 'web-mode-ac-sources-alist '("css" . (ac-source-words-in-buffer ac-source-css-property)))
        (add-to-list 'web-mode-ac-sources-alist '("html" . (ac-source-words-in-buffer ac-source-abbrev)))
        (add-to-list 'web-mode-ac-sources-alist '("jsx" . (ac-source-words-in-buffer ac-source-words-in-same-mode-buffers))))
    (setq web-mode-ac-sources-alist
          '(("css" . (ac-source-words-in-buffer ac-source-css-property))
            ("html" . (ac-source-words-in-buffer ac-source-abbrev))
            ("jsx" . (ac-source-words-in-buffer ac-source-words-in-same-mode-buffers)))))
  )
(add-hook 'web-mode-hook 'web-mode-hook)

;;; indentation
(setq web-mode-markup-indent-offset 2) ;html indentation
(setq web-mode-css-indent-offset 2)	;css indentation
(setq web-mode-code-indent-offset 2)	;script
(setq web-mode-indent-style 2)		;fix side effect for html indentation


;;; sass mode
(defun tmtxt/scss-setup ()
  "Setup scss mode"
  (setq-local css-indent-offset 2))
(setq scss-compile-at-save nil)
(add-hook 'scss-mode-hook 'tmtxt/scss-setup)
(add-hook 'scss-mode-hook 'tmtxt/prog-mode-setup)


;;; misc
;;; auto insert and tag when typing </
(setq nxml-slash-auto-complete-flag t)

;;; change indentation manually
(defun tmtxt/web-mode-change-indentation (indentation)
  (interactive (list (string-to-number (read-string "Indentation level: "))))
  (setq-local web-mode-markup-indent-offset indentation)
  (setq-local web-mode-css-indent-offset indentation)
  (setq-local web-mode-code-indent-offset indentation)
  (setq-local web-mode-indent-style indentation))

(provide 'tmtxt-web)
