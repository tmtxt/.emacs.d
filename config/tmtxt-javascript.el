;;; tmtxt-javascript.el --- Config for javascript

;;; Commentary:

;;; Load dependencies
(require 'js2-mode)
(require 'js2-refactor)
(require 'ac-js2)
(require 'flycheck)
(require 'json-mode)

;;; Code:

;;; js2
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 'auto-complete-mode)
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'tmtxt-paredit-nonlisp)
(add-hook 'js2-mode-hook 'js2-refactor-mode)
;; (add-hook 'js2-mode-hook 'js2-highlight-vars-mode)
(add-hook 'js2-mode-hook
          (lambda ()
            (js2-mode-hide-warnings-and-errors)
            (which-function-mode t)
            (add-hook 'before-save-hook 'tmtxt/edit-before-save-prog nil t)
            (tern-mode t)))
(setq-default js2-basic-offset 2
              js2-bounce-indent-p nil)

;;; mozrepl
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'js2-mode-hook 'moz-minor-mode)
(add-hook 'inferior-moz-mode-hook 'auto-complete-mode)

;;; tern
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(defun tmtxt/delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

;;; jsx
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-mode)
              (flycheck-add-mode 'javascript-eslint 'web-mode)
              (flycheck-select-checker 'javascript-eslint)
              ;; auto complete
              (auto-complete-mode 1)
              )))

;;; json
(add-hook 'json-mode-hook
          (lambda ()
            (setq-local json-reformat:indent-width 2)
            (setq-local js-indent-level 2)))

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
          ("\\(ret\\)urn"
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     ?▸ 'decompose-region)
                     nil)))
          ("ret\\(urn\\)"
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     ?▸ 'decompose-region)
                     nil))))))

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

(defun tmtxt/js-which-function ()
  (->> (which-function)
       (s-split "[.]")
       (-last-item)))

(provide 'tmtxt-javascript)
;;; tmtxt-javascript.el ends here
