;;; tmtxt-javascript.el --- Config for javascript

;;; Commentary:

;;; Load dependencies
(tmtxt/add-lib "prettier-js")

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
(require 'prettier-js)

;;; Code:

;;; modes
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(setq-default
 js2-basic-offset 2
 js2-bounce-indent-p nil
 )

;;; flycheck
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'js-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; relative executable eslint
;; (when (executable-find "eslint-project-relative")
;;   (setq flycheck-javascript-eslint-executable "eslint-project-relative"))
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

;;; js2
(dolist (f '(auto-complete-mode
             flycheck-mode
             tmtxt-paredit-nonlisp
             js2-mode-hide-warnings-and-errors
             which-function-mode
             tern-mode
             js2-refactor-mode
             toggle-truncate-lines
             ))
  (add-hook 'js2-mode-hook f))
(add-hook 'js2-mode-hook 'tmtxt/prog-mode-setup)
(add-hook 'js2-mode-hook
          (lambda () (setq fill-column 700))
          t)

;;; js2 jsx mode
(dolist (f '(tmtxt-paredit-nonlisp
             emmet-mode))
  (add-hook 'js2-mode-hook f))

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
          ;; ("\\(ret\\)urn"
          ;;  (0 (progn (compose-region (match-beginning 1) (match-end 1)
          ;;                            ?▸ 'decompose-region)
          ;;            nil)))
          ;; ("ret\\(urn\\)"
          ;;  (0 (progn (compose-region (match-beginning 1) (match-end 1)
          ;;                            ?▸ 'decompose-region)
          ;;            nil)))
          )))

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
        " * ${1:Function description.}\n *\n"
        (mapconcat (lambda (param)
                     (format
                      " * @param {${%d:Type of %s}} %s ${%d:}\n*\n"
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

(defun tmtxt/switch-to-web-mode ()
  (interactive)
  (web-mode))

(defun tmtxt/switch-to-js2-jsx-mode ()
  (interactive)
  (js2-jsx-mode))

(setq prettier-args '("--single-quote" "--parser" "flow" "--print-width" "100"))

(defun tmtxt/toggle-prettier-line-width ()
  (interactive)
  (let* ((current-line-width (-last-item prettier-args))
         (next-line-width (if (s-equals? current-line-width "80") "100" "80")))
    (setq prettier-args `("--single-quote" "--parser" "flow" "--print-width" ,next-line-width))))

(defun tmtxt/find-max-length-for-columns (regex-list beg end)
  (let ((cols-max-length '((1 . 0) (2 . 0) (3 . 0))))
    (dolist (re regex-list)
      (goto-char beg)

      (while (re-search-forward re nil t)
        (cl-loop
         for (col . max-length) in cols-max-length collect
         (when (match-beginning col)
           (let ((current-length (->
                                  (buffer-substring-no-properties
                                   (match-beginning col)
                                   (match-end col))
                                  (length))))
             (when (> current-length max-length)
               (add-to-list 'cols-max-length `(,col . ,current-length))))))))
    cols-max-length
    ))

(defun tmtxt/align-js-doc ()
  (interactive)

  (let* ((comments (js2-ast-root-comments js2-mode-ast)) beg end)
    (save-excursion
      (let ((node (-first-item comments)))
        (when (eq (js2-comment-node-format node) 'jsdoc)
          (setq beg (js2-node-abs-pos node)
                end (+ beg (js2-node-len node)))
          (save-restriction
            (narrow-to-region beg end)
            (let* ((regex-list (list js2-jsdoc-param-tag-regexp js2-jsdoc-typed-tag-regexp))
                   (cols-max-length) (tmtxt/find-max-length-for-columns beg end))

              ))))
      ;; (dolist (node comments)
      ;;   )
      )))

(provide 'tmtxt-javascript)
;;; tmtxt-javascript.el ends here
