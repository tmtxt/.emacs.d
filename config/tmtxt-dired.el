;;; dired --- config for using emacs as file manager

;;; Commentary:
;;; this is my config for dired mode
;;; its target is to replace macos as well as other os's default file explorer application

;;; Code:

;;; require
(require 'wdired)

;; some config
(setq-default
 dired-omit-mode t
 dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\."

 dired-details-hide-link-targets nil
 dired-recursive-deletes 'always
 dired-recursive-copies 'always
 dired-dwim-target t

 delete-by-moving-to-trash t
 global-auto-revert-non-file-buffers t)

(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode t)))

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
                     ('gnu/linux "xdg-open ")
                     ('windows-nt "open "))))
      (dolist (file files)
        (setq command (concat command (shell-quote-argument file) " ")))
      (async-shell-command command))))

(defun tmtxt/dired-open-current-directory ()
  "Open the current directory in Finder."
  (interactive)
  (save-window-excursion
    (let ((command (case system-type
                     ('darwin "open .")
                     ('gnu/linux "xdg-open .")
                     ('windows-nt "explorer.exe ."))))
      (async-shell-command command))))

(provide 'tmtxt-dired)
;;; tmtxt-dired.el ends here
