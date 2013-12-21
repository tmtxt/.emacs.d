;;; setting for javascript development

;;; include js2-mode
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;;; key bindings
(add-hook 'js-mode-hook
		  (lambda ()
			(define-key js-mode-map (kbd "M-j") 'js2-line-break)
			(define-key js-mode-map (kbd "C-M-?") 'ac-js2-jump-to-definition)
			(define-key js-mode-map (kbd "C-M-\"") 'js2-mark-defun)
			(define-key js-mode-map (kbd "C-M-:") 'js2-mode-toggle-hide-functions)
			(define-key js-mode-map (kbd "C-M->") 'js2-mode-toggle-element)))


(provide 'tmtxt-javascript)
