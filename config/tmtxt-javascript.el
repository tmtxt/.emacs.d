;;; setting for javascript development

(require 'js2-mode)
(require 'js2-refactor)
(require 'ac-js2)
(require 'flycheck)
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'js2-mode-hook 'auto-complete-mode)
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'tmtxt-paredit-nonlisp)
;; (add-hook 'js2-mode-hook 'js2-refactor-mode)
;; (add-hook 'js2-mode-hook 'js2-highlight-vars-mode)
(add-hook 'js2-mode-hook
          (lambda ()
            (js2-mode-hide-warnings-and-errors)
            (add-hook 'before-save-hook 'tmtxt/edit-before-save-prog nil t)
            (tern-mode t)))

(setq-default js2-basic-offset 2
              js2-bounce-indent-p nil)

;;; mozrepl integration
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'js2-mode-hook 'moz-minor-mode)

;;; tern
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(defun tmtxt/delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

;;; enable web mode and highlighting
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;;;
;; (flycheck-define-checker jsxhint-checker
;;   "A JSX syntax and style checker based on JSXHint."

;;   :command ("jsxhint" source)
;;   :error-patterns
;;   ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
;;   :modes (web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              ;; (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode)
              ;; auto complete
              (auto-complete-mode 1)
              ;; (tern-mode t)
              )))

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
          ("\\(ret\\)urn"
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     ?▸ 'decompose-region)
                     nil)))
          ("ret\\(urn\\)"
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     ?▸ 'decompose-region)
                     nil))))))

(add-hook 'json-mode-hook
          (lambda ()
            (setq-local json-reformat:indent-width 2)
            (setq-local js-indent-level 2)))
;;; coffeescript
;; (defun tmtxt/setup-coffee ()
;;   (auto-complete-mode t)
;;   (flycheck-mode t)
;;   (add-hook 'before-save-hook 'tmtxt/edit-before-save-prog nil t))
;; (tmtxt/set-up 'coffee-mode
;;   (add-hook 'coffee-mode-hook 'tmtxt/setup-coffee))

(tmtxt/set-up 'nodejs-repl
  (setq nodejs-repl-arguments
        '("--use-strict"
          "--es_staging"
          "--harmony"
          "--harmony_shipping"
          "--harmony_modules"
          "--harmony_arrays"
          "--harmony_array_includes"
          "--harmony_regexps"
          "--harmony_arrow_functions"
          "--harmony_proxies"
          "--harmony_sloppy"
          "--harmony_unicode"
          "--harmony_tostring"
          "--harmony_classes"
          "--harmony_object_literals"
          "--harmony_numeric_literals"
          "--harmony_strings"
          "--harmony_scoping"
          "--harmony_templates"))

  (defun tmtxt/send-region-nodejs-repl (start end)
    "Send region to `nodejs-repl' process."
    (interactive "r")
    (comint-send-region (get-process nodejs-repl-process-name)
                        start end)
    (comint-send-string (get-process nodejs-repl-process-name)
                        "\n")))

(provide 'tmtxt-javascript)
