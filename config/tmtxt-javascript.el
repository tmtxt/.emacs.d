;;; setting for javascript development

;;; include js2-mode
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;;; use json-mode instead of js2 for .json file
(tmtxt/set-up 'json-mode
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
  (add-hook 'json-mode-hook (lambda () (js2-minor-mode-exit)
                              (js2-mode-exit))))

;;; jshint
;;; requirements: nodejs, npm,
;;; install jshint via npm: npm install -g jshint
;; (tmtxt/add-lib "jshint-mode")
(tmtxt/set-up 'flycheck
  (add-hook 'js-mode-hook
            (lambda () (flycheck-mode t))))
;;; don't use this anymore since flycheck is a better version of flymake
;; (tmtxt/set-up 'flymake-jshint
;;   (setq jshint-configuration-path "~/.jshintrc"))
;; (add-hook 'js-mode-hook
;;      (lambda () (flymake-mode t)))

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
;; (add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(defun tmtxt/delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

;;; jsx-mode on melpa
;; (tmtxt/set-up 'jsx-mode
;;   (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
;;   (setq jsx-indent-level 2))

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

(provide 'tmtxt-javascript)
