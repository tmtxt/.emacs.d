;;; dired --- config for using emacs as file manager

;;; Commentary:
;;; this is my config for dired mode
;;; its target is to replace macos as well as other os's default file explorer application

;;; Code:

(tmtxt/add-lib "tmtxt-async-tasks")
(tmtxt/add-lib "tmtxt-dired-async")

;;; require
(require 'dired+)
(require 'dired-details+)
(require 'tmtxt-async-tasks)
(require 'tmtxt-dired-async)
(require 'wdired)

;; some config
(setq-default
 dired-omit-mode t
 dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\."
 dired-omit-extensions (->> dired-omit-extensions
                            (remove ".tp")
                            (remove ".lib"))

 dired-details-hide-link-targets nil
 dired-recursive-deletes 'always
 dired-recursive-copies 'always
 dired-dwim-target t

 delete-by-moving-to-trash t
 global-auto-revert-non-file-buffers t

 tda/get-files-size-command "du"
 tda/download-command "wget"
 )

(image-dired-display-image-mode)

;;; *Nix configuration - config that are shared between Mac and Linux
(tmtxt/in '(darwin gnu/linux)
  (setq-default
   ;; display directory first
   dired-listing-switches "--group-directories-first -alh"))

;;; Mac OS specific configuration
(tmtxt/in '(darwin)
  ;; Override this function to move file to trash
  (defun system-move-file-to-trash (file)
    "Delete files by moving to the folder emacs in Trash folder"
    (call-process (executable-find "trash")
                  nil 0 nil
                  file)))

;;; Linux specific configuration
(tmtxt/in '(gnu/linux)
  (setq trash-directory "~/.local/share/Trash/files/emacs"))

;;; Windows specific configuration
(tmtxt/in '(windows-nt)
  (setq-default ls-lisp-dirs-first t))

;;; Othrer util functions
(defun tmtxt/dired-do-shell-open ()
  "Open file/marked files by default program."
  (interactive)
  (save-window-excursion
    (let ((files (dired-get-marked-files nil current-prefix-arg))
          (command (case system-type
                     ('darwin "open ")
                     ('gnu/linux "xdg-open "))))
      (dolist (file files)
        (setq command (concat command (shell-quote-argument file) " ")))
      (async-shell-command command))))

(defun tmtxt/dired-do-shell-unmount-device ()
  "Unmount device"
  (interactive)
  (save-window-excursion
    (let ((command (case system-type
                     ('darwin "diskutil unmount")
                     ('gnu/linux "unmount"))))
      (dired-do-async-shell-command
       command current-prefix-arg (dired-get-marked-files t current-prefix-arg)))))

(defun tmtxt/dired-open-current-directory ()
  "Open the current directory in Finder."
  (interactive)
  (save-window-excursion
    (let ((command (case system-type
                     ('darwin "open .")
                     ('gnu/linux "xdg-open ."))))
      (async-shell-command command))))


;;; fast renaming for wdired
(defun tmtxt/mark-file-name-for-rename ()
  "Mark file name on current line except its extension"
  (interactive)

  ;; get the file file name first
  ;; full-name: full file name
  ;; extension: extension of the file
  ;; base-name: file name without extension
  (let ((full-name (file-name-nondirectory (dired-get-filename)))
        extension base-name)

    ;; check if it's a dir or a file
    ;; TODO not use if, use switch case check for symlink
    (if (file-directory-p full-name)
        (progn
          ;; if file name is directory, mark file name should mark the whole
          ;; file name
          (call-interactively 'end-of-line) ;move the end of line
          (backward-char (length full-name)) ;back to the beginning
          (set-mark (point))
          (forward-char (length full-name)))
      (progn
        ;; if current file is a file, mark file name mark only the base name,
        ;; exclude the extension
        (setq extension (file-name-extension full-name))
        (setq base-name (file-name-sans-extension full-name))
        (call-interactively 'end-of-line)
        (backward-char (length full-name))
        (set-mark (point))
        (forward-char (length base-name))))))

(defun tmtxt/mark-file-name-forward ()
  "Mark file name on the next line"
  (interactive)
  (deactivate-mark)
  (next-line)
  (tmtxt/mark-file-name-for-rename))

(defun tmtxt/mark-file-name-backward ()
  "Mark file name on the next line"
  (interactive)
  (deactivate-mark)
  (previous-line)
  (tmtxt/mark-file-name-for-rename))

(provide 'tmtxt-dired)
;;; tmtxt-dired.el ends here
