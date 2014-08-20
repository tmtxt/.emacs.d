;;; some config for easy editing

(require 'textmate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTF-8
;;; set the encoding for emacs and external program to interact with each other
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq default-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto pair the brackets
(tmtxt/set-up 'autopair
  (autopair-global-mode 1)
  (setq autopair-autowrap t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; select all line
(defun tmtxt/select-all-line ()
  "select all line and put the cursor at the end of that line"
  (interactive)
  (tmtxt/back-to-indentation-or-line-beginning)
  (set-mark-command nil)
  (move-end-of-line nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; copy/cut whole line if no region is selected
;;; http://www.emacswiki.org/emacs/WholeLineOrRegion
(dolist (command (list 'kill-ring-save 'kill-region
                       'clipboard-kill-ring-save
                       'clipboard-kill-region))
  (put command 'interactive-form
       '(interactive
         (if (use-region-p)
             (list (region-beginning) (region-end))
           (list (line-beginning-position) (line-beginning-position 2))))))
;;; Because they set mark if the region is not active
(defadvice kill-ring-save (after pop-spurious-mark activate)
  (unless (use-region-p)
    (pop-mark)))
(defadvice kill-region (after pop-spurious-mark activate)
  (unless (use-region-p)
    (pop-mark)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; indent current region if active, otherwise indent all buffer
(defun tmtxt/indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))
(defun tmtxt/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Selected region indented."))
      (progn
		(tmtxt/indent-buffer)
		(message "Buffer indented.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; edit file as root privileges (a bit slow at when first open)
;; http://emacs-fu.blogspot.com/2013/03/editing-with-root-privileges-once-more.html
(defun tmtxt/find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Find file as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yasnippet
;;; should be loaded before auto complete so that they can work together
(tmtxt/set-up 'yasnippet
  (yas-global-mode 1)
  (setq yas/root-directory "~/.emacs.d/data/yasnippet/snippets")
  (yas/load-directory yas/root-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fix paredit and comment-dwim conflict
(add-hook 'paredit-mode-hook (lambda () (define-key paredit-mode-map (kbd "M-;") nil)))
(defadvice comment-dwim (around lisp-specific activate)
    "Use `paredit-comment-dwim', but only in lisp code."
    (if (member major-mode '(lisp-mode emacs-lisp-mode clojure-mode scheme-mode))
        (call-interactively 'paredit-comment-dwim)
      (message "normal")
      ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; untabify current buffer
(defun tmtxt-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; insert sample lorem text
(defun tmtxt-lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; insert current time
(defun tmtxt-insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; autocomplete from ispell
(custom-set-variables
 '(ac-ispell-requires 4))
(eval-after-load "auto-complete"
  '(progn
	 (ac-ispell-setup)))
(defun my/enable-ac-ispell ()
  (add-to-list 'ac-sources 'ac-source-ispell))
(add-hook 'markdown-mode-hook 'my/enable-ac-ispell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc for prog mode
(defun tmtxt/edit-before-save-prog ()
  (delete-trailing-whitespace)
  (tmtxt-untabify-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some minor config
(delete-selection-mode 1)				;delete selection mode
(setq next-line-add-newlines t)			;auto new line
(setq-default fill-column 80)			;fill column 80
(setq-default indent-tabs-mode nil)
(tmtxt/set-up 'undo-tree (global-undo-tree-mode)) ;undo tree
(tmtxt/enable '(narrow-to-region set-goal-column upcase-region downcase-region))
(setq sentence-end-double-space nil)	;one space not end setence
(setq shift-select-mode nil)			;not use shift to select
(setq mouse-yank-at-point t)
(setq whitespace-line-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; finally provide the library
(provide 'tmtxt-editing)
