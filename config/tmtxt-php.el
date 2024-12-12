;;; config for php coding

(defun tmtxt/setup-php-web-mode ()
  "Setup php using web mode"

  ;; enable web mode
  (web-mode)

  ;; indentation
  (make-local-variable 'web-mode-code-indent-offset)
  (make-local-variable 'web-mode-markup-indent-offset)
  (make-local-variable 'web-mode-comment-style)
  (setq web-mode-comment-style 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)

  ;; auto complete
  (add-to-list 'web-mode-ac-sources-alist
               '("php" ac-source-words-in-buffer
                 ac-source-words-in-same-mode-buffers
                 ac-source-dictionary))

  (tmtxt/setup-php))



;;; setting up php mode
(defun tmtxt/setup-php ()
  (tmtxt/prog-mode-setup)

  ;; undo
  (make-local-variable 'undo-outer-limit)
  (setq
   undo-outer-limit 100000000
   fill-column 500
   )

  ;; paredit
  (tmtxt-paredit-nonlisp)

  ;; flycheck
  (flycheck-select-checker 'tmtxt-php)
  (flycheck-mode t))

;;; function for switching between web mode and php mode
(defun tmtxt/switch-php-mode ()
  "Dynamically switch between web-mode and php-mode"
  (interactive)
  (if (eq major-mode 'php-mode)
      (tmtxt/setup-php-web-mode)
    (tmtxt/setup-php-php-mode)))

;;; default web mode for php file
(add-to-list 'auto-mode-alist '("\\.php$" . tmtxt/setup-php-web-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . tmtxt/setup-php-web-mode))

;;; finally provide the library
(provide 'tmtxt-php)
