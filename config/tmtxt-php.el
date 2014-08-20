;;; config for php coding

(flycheck-define-checker tmtxt-php
  "A PHP syntax checker using the PHP command line interpreter.

See URL `http://php.net/manual/en/features.commandline.php'."
  :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
            "-d" "log_errors=0" source)
  :error-patterns
  ((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
          (message) " in " (file-name) " on line " line line-end))
  :modes (php-mode php+-mode web-mode))

;;; setting up php mode
(defun tmtxt/setup-php ()
  ;; enable web mode
  (web-mode)

  ;; delete trailing whitespace
  (add-hook 'before-save-hook 'tmtxt/edit-before-save-prog nil t)

  ;; indentation
  (make-local-variable 'web-mode-code-indent-offset)
  (make-local-variable 'web-mode-markup-indent-offset)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)

  ;; undo
  (make-local-variable 'undo-outer-limit)
  (setq undo-outer-limit 100000000)

  ;; comment
  (make-local-variable 'web-mode-comment-style)
  (setq web-mode-comment-style 2)

  ;; paredit
  (tmtxt-paredit-nonlisp)

  ;; flycheck
  (flycheck-select-checker 'tmtxt-php)
  (flycheck-mode t))

;;; auto enable php mode for .php and .inc files
(add-to-list 'auto-mode-alist '("\\.php$" . tmtxt/setup-php))
(add-to-list 'auto-mode-alist '("\\.inc$" . tmtxt/setup-php))

;;; finally provide the library
(provide 'tmtxt-php)
