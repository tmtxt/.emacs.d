;;; tmtxt-util -- some of my utility functions

;;; OS-specific stuff
(defmacro tmtxt/in (systems &rest body)
  "Run BODY if `system-type' is in the list of SYSTEMS.

Example:
(tmtxt/in '(darwin) something here)
(tmtxt/in '(gnu/linux) something here)"
  (declare (indent 1))
  `(when (member system-type ,systems)
     ,@body))

(defun tmtxt/prog-mode-setup ()
  "Some util for prog modes"
  (add-hook
   'before-save-hook
   (lambda ()
     (delete-trailing-whitespace))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a replacement for (require 'xxx)
;;; https://github.com/ubolonton/.emacs.d/blob/master/config/ublt-editing.el
;;; usage:
;; (tmtxt/set-up 'xxx
;;   body)
;;; try to load (require the feature xxx) and then run the body (those commands
;;; that go with xxx feature)
;;; if can not load xxx feature, ignore it and continue running the rest of the
;;; current file
(defvar tmtxt/ok-features ())
(defvar tmtxt/error-features ())
;;; XXX: Hmm
(defun tmtxt/require (feature &optional filename noerror)
  (if noerror
      (condition-case err
          (progn
            (let ((name (require feature filename)))
              (add-to-list 'tmtxt/ok-features feature t)
              (message "Feature `%s' ok" feature)
              name))
        (error
         (setq tmtxt/error-features (plist-put tmtxt/error-features feature err))
         (message "Feature `%s' failed" feature)
         nil))
    (require feature filename)))
(defmacro tmtxt/set-up (feature &rest body)
  "Try loading the feature, running BODY afterward, notifying
user if not found. This is mostly for my customizations, since I
don't want a feature failing to load to affect other features in
the same file. Splitting everything out would result in too many
files."
  (declare (indent 1))
  `(let ((f (if (stringp ,feature) (intern ,feature) ,feature)))
     (when (tmtxt/require f nil t)
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; enable all disabled functions in funcs list
(defun tmtxt/enable (funcs)
  (dolist (f funcs)
    (put f 'disabled nil)))

;;; turn on and off function
(defvar tmtxt/on-fns (make-hash-table))
(defun tmtxt/on-fn (minor-mode-fn)
  (let ((fn (gethash minor-mode-fn tmtxt/on-fns)))
    (if fn fn
      (puthash minor-mode-fn
               `(lambda () (,minor-mode-fn +1))
               tmtxt/on-fns))))
(defvar tmtxt/off-fns (make-hash-table))
(defun tmtxt/off-fn (minor-mode-fn)
  (let ((fn (gethash minor-mode-fn tmtxt/off-fns)))
    (if fn fn
      (puthash minor-mode-fn
               `(lambda () (,minor-mode-fn -1))
               tmtxt/off-fns))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; show message in minibuffer
(defun tmtxt/minibuffer-message (&rest args)
  "Show a message in the minibuffer without logging. Useful for
transient messages like error messages when hovering over syntax
errors."
  (let ((message-log-max nil))
    (apply #'message args)))

;;; last, provide the library
(provide 'tmtxt-util)
