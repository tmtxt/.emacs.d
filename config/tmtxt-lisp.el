;;; some of the code is from emacs starter kit

(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'tmtxt-remove-elc-on-save)
(add-hook 'emacs-lisp-mode-hook 'tmtxt-pretty-lambdas)
(add-hook 'emacs-lisp-mode-hook 'tmtxt-pretty-fn)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'emacs-lisp-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

(defun tmtxt-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
			(lambda ()
			  (if (file-exists-p (concat buffer-file-name "c"))
				  (delete-file (concat buffer-file-name "c"))))))

(defun tmtxt-pretty-lambdas ()
  "prettify lambda"
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun tmtxt-pretty-fn ()
  ;; prettify the function
  (font-lock-add-keywords nil `(("(\\(\\<fn\\>\\)"
								 (0 (progn (compose-region (match-beginning 1)
														   (match-end 1)
														   "\u0192"
														   'decompose-region)))))))

;;; key bindings
(define-key lisp-mode-map (kbd "C-c C-r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(provide 'tmtxt-lisp)
