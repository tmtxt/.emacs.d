;;; config for working with web page (html, xml, js, ts...)

;;; required library
(require 'tmtxt-util)

;;; emmet mode
(require 'js2-mode)
(require 'flycheck)
(require 'json-mode)
(require 'emmet-mode)

(setq-default
 js2-basic-offset 2
 js2-bounce-indent-p nil)

;;; auto activate for js
(dolist (f '(auto-complete-mode
             flycheck-mode
             js2-mode-hide-warnings-and-errors
             ; prettier-mode
             tmtxt-paredit-nonlisp
             tmtxt/prog-mode-setup
             toggle-truncate-lines
             js2-highlight-vars-mode
             which-function-mode))
  (add-hook 'js2-mode-hook f))
(dolist (f '(auto-complete-mode
             flycheck-mode
             ; prettier-mode
             tmtxt-paredit-nonlisp
             tmtxt/prog-mode-setup
             toggle-truncate-lines))
  (add-hook 'js-mode-hook f))
(add-hook 'js2-mode-hook (lambda () (setq fill-column 700)) t)

;;; jsx config
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              (flycheck-mode)
              (flycheck-select-checker 'javascript-eslint)
              (auto-complete-mode 1)
              ; (prettier-mode)
              )))

(add-hook 'json-mode-hook
          (lambda ()
            (setq-local json-reformat:indent-width 2)
            (setq-local js-indent-level 2)
            (flycheck-mode)))

(dolist (mode '(js-mode js2-mode web-mode))
  (font-lock-add-keywords
   mode `(
          ("\\(function\\)"
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     ?ƒ 'decompose-region)
                     nil)))
          ("\\(yi\\)eld"
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     ?γ 'decompose-region)
                     nil)))
          ("yi\\(eld\\)"
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     ?ζ 'decompose-region)
                     nil)))
          )))

;;; auto start on sgml and css mode
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook 'emmet-mode)

;;; emmet mode indentation
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;indent 2 spaces.

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
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.csproj$" . web-mode))

;;; disable rainbow-mode and whitespace-mode when use web-mode
(defun web-mode-hook ()
  "Config for working with web mode"

  (tmtxt/prog-mode-setup)

  ;; disable rainbow, whitespace, idle highlight, font lock mode
  (whitespace-mode 0)
  (font-lock-mode 1)
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
