;;; setting for javascript development

;;; include js2-mode
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;;; associate with js mode
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))


(eval-after-load 'js
  '(progn
	 ;;; integrate paredit with js mode
	 (define-key js-mode-map "{" 'paredit-open-curly)
	 (define-key js-mode-map "}" 'paredit-close-curly-and-newline)
	 (add-hook 'js-mode-hook 'tmtxt-paredit-nonlisp)

	 ;; indent level
	 (setq js-indent-level 2)

	 ;; fixes problem with pretty function font-lock
	 (define-key js-mode-map (kbd ",") 'self-insert-command)

	 ;; pretty symbol
	 (font-lock-add-keywords
	  'js-mode `(("\\(function *\\)("
				  (0 (progn (compose-region (match-beginning 1)
											(match-end 1) "\u0192")
							nil)))))))

;;; beautify json
;;; require python installed
(defun tmtxt-beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

;;; key bindings
(add-hook
 'js-mode-hook
 (lambda ()
   (define-key js-mode-map (kbd "M-j") 'js2-line-break)
   (define-key js-mode-map (kbd "C-M-?") 'ac-js2-jump-to-definition)
   (define-key js-mode-map (kbd "C-M-\"") 'js2-mark-defun)
   (define-key js-mode-map (kbd "C-M-:") 'js2-mode-toggle-hide-functions)
   (define-key js-mode-map (kbd "C-M->") 'js2-mode-toggle-element)))

;;; mozrepl integration
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'javascript-mode-hook 'moz-minor-mode)
(add-hook 'js-mode-hook 'moz-minor-mode)

(provide 'tmtxt-javascript)
