;;; some config for easy editing

(require 'textmate)
(require 'yasnippet)
(require 'autopair)
(require 'undo-tree)

;;; UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq default-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;;; enable modes
(autopair-global-mode 1)
(delete-selection-mode 1)
(global-undo-tree-mode)
(global-subword-mode t)
(global-auto-revert-mode 1)
(toggle-text-mode-auto-fill)

;;; minor config
(setq-default
 next-line-add-newlines t
 fill-column 100
 indent-tabs-mode nil
 sentence-end-double-space nil
 shift-select-mode nil
 mouse-yank-at-point t
 whitespace-line-column 80)

(defun tmtxt-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))
(add-hook 'prog-mode-hook 'tmtxt-local-comment-auto-fill)

;;; select all line
(defun tmtxt/select-all-line ()
  "select all line and put the cursor at the end of that line"
  (interactive)
  (tmtxt/back-to-indentation-or-line-beginning)
  (set-mark-command nil)
  (move-end-of-line nil))

;;; copy/cut whole line if no region is selected
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

;;; edit file as root privileges (a bit slow at when first open)
(defun tmtxt/find-file-as-root ()
  "Like `find-file, but automatically edit the file with root-privileges (using tramp/sudo), if the file is not writable by user."
  (interactive)
  (let ((file (ido-read-file-name "Find file as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

;;; yasnippet
;;; should be loaded before auto complete so that they can work together
(add-to-list 'yas-snippet-dirs "~/.emacs.d/data/yasnippet/snippets")
(yas-global-mode 1)
(yas-reload-all)

;; fix paredit and comment-dwim conflict
(add-hook 'paredit-mode-hook (lambda () (define-key paredit-mode-map (kbd "M-;") nil)))
(defadvice comment-dwim (around lisp-specific activate)
  "Use `paredit-comment-dwim', but only in lisp code."
  (if (member major-mode '(lisp-mode emacs-lisp-mode clojure-mode scheme-mode))
      (call-interactively 'paredit-comment-dwim)
    (message "normal")
    ad-do-it))

(defun tmtxt/untabify-buffer ()
  "Untabify whole buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(defun tmtxt/lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun tmtxt/change-indentation-locally (indentation)
  "Change indentation locally"
  (interactive (list (string-to-number (read-string "Indentation level: "))))
  (setq-local c-basic-offset indentation))

(defun tmtxt/escape-quotes (φbegin φend)
  "Replace 「\"」 by 「\\\"」 in current line or text selection.
See also: `xah-unescape-quotes'
URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2015-05-04"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region φbegin φend)
      (goto-char (point-min))
      (while (search-forward "\"" nil t)
        (replace-match "\\\"" 'FIXEDCASE 'LITERAL)))))

(defun tmtxt/unescape-quotes (φbegin φend)
  "Replace  「\\\"」 by 「\"」 in current line or text selection.
See also: `xah-escape-quotes'
URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2015-05-04"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region φbegin φend)
      (goto-char (point-min))
      (while (search-forward "\\\"" nil t)
        (replace-match "\"" 'FIXEDCASE 'LITERAL)))))

(defun tmtxt/insert-tab-as-spaces ()
  "Insert a tab character and then convert it to spaces"
  (interactive)
  (let (start end)
    (setq start (point))
    (insert "\t")
    (setq end (point))
    (untabify start end)
    ))

(provide 'tmtxt-editing)
