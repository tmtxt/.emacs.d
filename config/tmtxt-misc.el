;;; turn of paredit for non lisp mode
(defun tmtxt-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

;;; add watch words
(defun tmtxt-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'tmtxt-add-watchwords)

;;; auto fill comment
(defun tmtxt-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))
(add-hook 'prog-mode-hook 'tmtxt-local-comment-auto-fill)

(provide 'tmtxt-misc)
