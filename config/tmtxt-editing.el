;;; some config for easy editing

;;; UTF-8
;;; set the encoding for emacs and external program to interact with each other
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq default-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;;; auto pair the brackets
(tmtxt/set-up 'autopair
  (autopair-global-mode 1)
  (setq autopair-autowrap t))

;;; select all line
(defun tmtxt/select-all-line ()
  "select all line and put the cursor at the end of that line"
  (interactive)
  (back-to-indentation)
  (set-mark-command nil)
  (move-end-of-line nil))
(global-set-key (kbd "C-c C-a") 'tmtxt/select-all-line)

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
(global-set-key (kbd "C-M-\\") 'tmtxt/indent-region-or-buffer)

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
;; bind it to C-x F
(global-set-key (kbd "C-x F") 'tmtxt/find-file-as-root)

;;; yasnippet
;;; should be loaded before auto complete so that they can work together
(tmtxt/set-up 'yasnippet
  (yas-global-mode 1)
  (setq yas/root-directory "~/.emacs.d/data/yasnippet/snippets")
  (yas/load-directory yas/root-directory))

;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;;; undo tree
(tmtxt/set-up 'undo-tree
  (global-undo-tree-mode))

;;; disable fly-spell mode by default
(eval-after-load "flyspell"
  '(defun flyspell-mode (&optional arg)))

;;; enable some disabled functions
(tmtxt/enable '(narrow-to-region set-goal-column upcase-region downcase-region))

;;; Some minor config
;;; delete selection mode, let emacs behave same as normal text editor
(delete-selection-mode 1)
;;; auto add new line if the cursor is at the end of buffer
(setq next-line-add-newlines t)
;;; tab
(setq-default tab-width 4
			  indent-tabs-mode t)
(global-set-key (kbd "C-m") 'newline-and-indent)

;;; finally provide the library
(provide 'tmtxt-editing)
