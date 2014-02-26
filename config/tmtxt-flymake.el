;;; config for flymake
;;; this file should be loaded before those file like cc, php,... so that those
;;; files can use flymake functions defined here

(require 'flymake)

;;; auto enable flymake if available
;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;;; setup flymake for input mode
(defun tmtxt/setup-flymake-for-mode (mode)
  "Setup flymake for the input mode"
  (define-key mode (kbd "M-m") 'flymake-goto-next-error)
  (define-key mode (kbd "M-,") 'flymake-goto-prev-error))

;;; print the error in the minibuffer when goto that error
(defun tmtxt/flymake-err-at (pos)
  (let ((overlays (overlays-at pos)))
    (remove nil
            (mapcar (lambda (overlay)
                      (and (overlay-get overlay 'flymake-overlay)
                           (overlay-get overlay 'help-echo)))
                    overlays))))
(defface tmtxt/flymake-message-face
  `((t (:inherit font-lock-keyword-face)))
  "Face for flymake message echoed in the minibuffer.")
(defun tmtxt/flymake-err-echo ()
  "Echo flymake error message in the minibuffer (not saving to *Messages*)."
  (tmtxt/minibuffer-message "%s"
             (propertize (mapconcat 'identity
                                    (tmtxt/flymake-err-at (point)) "\n")
                         'face 'tmtxt/flymake-message-face)))
;;; advice for functions
(defadvice flymake-goto-next-error (after display-message activate)
  (tmtxt/flymake-err-echo))
(defadvice flymake-goto-prev-error (after display-message activate)
  (tmtxt/flymake-err-echo))

;;; finally provide the library
(provide 'tmtxt-flymake)
