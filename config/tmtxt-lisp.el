;;; some of the code is from emacs starter kit

(require 'flycheck)
(require 'clojure-mode)
(require 'cider)
(require 'ac-cider)
(require 'quack)

;;; LISP & EMACS LISP
;;; hook
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(dolist (f '(enable-paredit-mode
             turn-on-eldoc-mode
             elisp-slime-nav-mode
             tmtxt/remove-elc-on-save
             tmtxt/add-elisp-font-lock
             tmtxt/prog-mode-setup))
  (add-hook 'emacs-lisp-mode-hook f))

;;; flycheck setup
(flycheck-package-setup)

(defun tmtxt/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(defun tmtxt/add-elisp-font-lock ()
  "Add font lock keywords for emacs lisp"
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))


;;; CLOJURE & CLOJURESCRIPT
(add-hook 'clojure-mode-hook 'tmtxt/add-clj-font-lock)
(add-hook 'clojurescript-mode-hook 'tmtxt/add-clj-font-lock)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'tmtxt/prog-mode-setup)

(defun tmtxt/add-clj-font-lock ()
  "Add font lock keywords for clojure"
  (font-lock-add-keywords
   nil `(("(\\(\\<fn\\>\\)"
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1)
                                    "\u0192"
                                    'decompose-region)))))))

(defun tmtxt/switch-to-cider-repl ()
  "Switch directly to cider repl buffer if exist, otherwise, connect to a new one"
  (interactive)
  ;; check whether cider repl buffer exists
  (let* ((cider-buffer? (lambda (buffer)
                          (->> buffer
                               (buffer-name)
                               (s-contains? "cider-repl"))))
         (cider-buffers (->> (buffer-list)
                             (-filter cider-buffer?))))
    (if cider-buffers
        (switch-to-buffer (first cider-buffers))
      (call-interactively 'cider-connect))))

(dolist (f '(ac-flyspell-workaround
             ac-cider-setup
             ac-cider-setup))
  (add-hook 'cider-mode-hook f))
(add-to-list 'ac-modes 'cider-mode)
(add-to-list 'ac-modes 'cider-repl-mode)

(defun tmtxt/set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'tmtxt/set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'tmtxt/set-auto-complete-as-completion-at-point-function)


;; SCHEME
;; The binary of your interpreter
(setq scheme-program-name "mit-scheme")
;; This hook lets you use your theme colours instead of quack's ones.
(defun tmtxt/scheme-mode-quack-hook ()
  (setq quack-fontify-style 'emacs))
(add-hook 'scheme-mode-hook 'tmtxt/scheme-mode-quack-hook)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)

(provide 'tmtxt-lisp)
