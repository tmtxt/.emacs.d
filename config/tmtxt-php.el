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

(defun tmtxt/setup-php-php-mode ()
  "Setup php using php mode"

  ;; enable php mode
  (php-mode)

  ;; sub word
  (subword-mode 1)

  ;; indentation
  (setq-local c-basic-offset 2)

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
