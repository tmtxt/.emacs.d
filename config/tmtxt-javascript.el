;;; tmtxt-javascript.el --- Config for javascript

;;; Commentary:

;;; Load dependencies
(require 'js2-mode)
(require 'js2-refactor)
(require 'ac-js2)
(require 'flycheck)
(require 'json-mode)
(require 'js-doc)
(require 'tern)
(require 'tern-auto-complete)
(require 'nodejs-repl)
(require 'moz)

;;; Code:

;;; modes
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(setq-default
 js2-basic-offset 2
 js2-bounce-indent-p nil
 )

;;; js2
(dolist (f '(auto-complete-mode
             flycheck-mode
             tmtxt-paredit-nonlisp
             js2-mode-hide-warnings-and-errors
             which-function-mode
             tern-mode
             js2-refactor-mode
             ))
  (add-hook 'js2-mode-hook f))
(add-hook 'js2-mode-hook 'tmtxt/prog-mode-setup)

;;; json
(add-hook 'json-mode-hook
          (lambda ()
            (setq-local json-reformat:indent-width 2)
            (setq-local js-indent-level 2)
            (flycheck-mode)))

;;; jsx
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              (flycheck-mode)
              (flycheck-add-mode 'javascript-eslint 'web-mode)
              (flycheck-select-checker 'javascript-eslint)
              (auto-complete-mode 1)
              )))


;;; mozrepl
;; (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'js2-mode-hook 'moz-minor-mode)
(add-hook 'inferior-moz-mode-hook 'auto-complete-mode)


(defun tmtxt/delete-tern-process ()
  "Delete the currently run tern process"
  (interactive)
  (delete-process "Tern"))


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

(setq-default nodejs-repl-arguments
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
                      "\n"))

(defun tmtxt/js-which-function ()
  "Identical to which-function but strip to get only the function name"
  (->> (which-function)
       (s-split "[.]")
       (-last-item)))

(defun tmtxt/js-doc-insert-function-doc-snippet ()
  "Insert JsDoc style comment of the function with yasnippet. Taken from js-doc code to change the snippet"
  (interactive)

  (with-eval-after-load 'yasnippet
    (js-doc--beginning-of-defun)

    (let ((metadata (js-doc--function-doc-metadata))
          (field-count 1))
      (yas-expand-snippet
       (concat
        js-doc-top-line
        " * ${1:Function description.}\n"
        (mapconcat (lambda (param)
                     (format
                      " * @param {${%d:Type of %s}} %s ${%d:}\n"
                      (incf field-count)
                      param
                      param
                      (incf field-count)))
                   (cdr (assoc 'params metadata))
                   "")
        (when (assoc 'returns metadata)
          (format
           " * @returns {${%d:Return Type}} ${%d:}\n"
           (incf field-count)
           (incf field-count)))
        (when (assoc 'throws metadata)
          (format
           " * @throws {${%d:Exception Type}} ${%d:}\n"
           (incf field-count)
           (incf field-count)))
        js-doc-bottom-line)))))

(provide 'tmtxt-javascript)
;;; tmtxt-javascript.el ends here
