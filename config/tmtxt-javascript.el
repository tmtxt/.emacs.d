;;; setting for javascript development

;;; include js2-mode
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;;; use json-mode instead of js2 for .json file
(tmtxt/set-up 'json-mode
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
  (add-hook 'json-mode-hook (lambda () (js2-minor-mode 0)
                              (js2-mode-exit))))

;;; jshint
(tmtxt/set-up 'flycheck
  (add-hook 'js-mode-hook
            (lambda () (flycheck-mode t))))

;;; beautify json
;;; require python installed
(defun tmtxt-beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

;;; mozrepl integration
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'javascript-mode-hook 'moz-minor-mode)
(add-hook 'js-mode-hook 'moz-minor-mode)

;;; tern
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
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
(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode)
              ;; auto complete
              (auto-complete-mode 1)
              ;; (tern-mode t)
              )))

(eval-after-load 'js
  '(progn
	 ;;; integrate paredit with js mode

     (add-hook 'js-mode-hook 'tmtxt-paredit-nonlisp)

     ;; indent level
     (setq js-indent-level 2)

     ;; fixes problem with pretty function font-lock
     (define-key js-mode-map (kbd ",") 'self-insert-command)

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
                          nil))))))))



;;; require js2-refactor mode
(tmtxt/set-up 'js2-refactor)

;;; enable hide/show
(add-hook 'js-mode-hook (lambda () (hs-minor-mode 1)))

;;; util func when save
(defun tmtxt/js-save-util ()
  (add-hook 'before-save-hook 'tmtxt/edit-before-save-prog nil t))
(add-hook 'js-mode-hook 'tmtxt/js-save-util)

(add-hook 'js-mode-hook 'which-function-mode)

;;; coffeescript
(defun tmtxt/setup-coffee ()
  (flycheck-mode t)
  (add-hook 'before-save-hook 'tmtxt/edit-before-save-prog nil t))
(tmtxt/set-up 'coffee-mode
  (add-hook 'coffee-mode-hook 'tmtxt/setup-coffee))

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
