;;; tmtxt-util -- some of my utility functions

;;; OS-specific stuff
(defmacro tmtxt/in (systems &rest body)
  "Run BODY if `system-type' is in the list of SYSTEMS.

Example:
(tmtxt/in '(darwin) body here)
(tmtxt/in '(gnu/linux) body here)
(tmtxt/in '(windows-nt) body-here)"
  (declare (indent 1))
  `(when (member system-type ,systems)
     ,@body))

(defun tmtxt/prog-mode-setup ()
  "Some util for prog modes"
  (add-hook
   'before-save-hook
   (lambda ()
     (delete-trailing-whitespace))))

(defun tmtxt-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(defun tmtxt/minibuffer-message (&rest args)
  "Show a message in the minibuffer without logging. Useful for
transient messages like error messages when hovering over syntax
errors."
  (let ((message-log-max nil))
    (apply #'message args)))

(defun tmtxt-create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun tmtxt/insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

;;; last, provide the library
(provide 'tmtxt-util)
