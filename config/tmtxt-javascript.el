;;; tmtxt-javascript.el --- Config for javascript

;;; Commentary:

;;; Load dependencies

(require 'js2-mode)
(require 'ac-js2)
(require 'flycheck)
(require 'json-mode)
(require 'js-doc)
;; (require 'tern)
;; (require 'tern-auto-complete)
;; (require 'moz)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General JS config

;;; Auto mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;;; Some config
(setq-default
 js2-basic-offset 2
 js2-bounce-indent-p nil)

;;; Auto activate
(dolist (f '(auto-complete-mode
             flycheck-mode
             js2-mode-hide-warnings-and-errors
             prettier-mode
             tmtxt-paredit-nonlisp
             tmtxt-paredit-nonlisp
             tmtxt/prog-mode-setup
             toggle-truncate-lines
             which-function-mode))
  (add-hook 'js2-mode-hook f))
(add-hook 'js2-mode-hook (lambda () (setq fill-column 700)) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flycheck config

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'js-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)

;;; auto switch between global and local eslint
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
              )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; json-mode config
(add-hook 'json-mode-hook
          (lambda ()
            (setq-local json-reformat:indent-width 2)
            (setq-local js-indent-level 2)
            (flycheck-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mozrepl
;; (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
;; (add-hook 'js2-mode-hook 'moz-minor-mode)
;; (add-hook 'inferior-moz-mode-hook 'auto-complete-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Font lock
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
          ;; ("\\(ret\\)urn"
          ;;  (0 (progn (compose-region (match-beginning 1) (match-end 1)
          ;;                            ?▸ 'decompose-region)
          ;;            nil)))
          ;; ("ret\\(urn\\)"
          ;;  (0 (progn (compose-region (match-beginning 1) (match-end 1)
          ;;                            ?▸ 'decompose-region)
          ;;            nil)))
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other util functions
(defun tmtxt/delete-tern-process ()
  "Delete the currently run tern process"
  (interactive)
  (delete-process "Tern"))

(defun tmtxt/switch-to-web-mode ()
  (interactive)
  (web-mode))

(defun tmtxt/switch-to-js2-jsx-mode ()
  (interactive)
  (js2-jsx-mode))

(provide 'tmtxt-javascript)
;;; tmtxt-javascript.el ends here
