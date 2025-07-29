;;; dired --- config for using emacs as file manager
;;; this is my config for dired mode
;;; its target is to replace macos as well as other os's default file explorer application

(require 'wdired)
(require 'dired-x)

;; some config
(setq-default
 dired-omit-mode t
 dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\."
 dired-omit-extensions (delete ".dll" dired-omit-extensions)
 completion-ignored-extensions (delete ".dll" completion-ignored-extensions)

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
  (setq-default
   dired-omit-mode nil
   ls-lisp-dirs-first t))

;;; Othrer util functions
(defun tmtxt/dired-do-shell-open ()
  "Open file/marked files by default program."
  (interactive)
  (save-window-excursion
    (let ((files (dired-get-marked-files nil current-prefix-arg))
          (command (cond ((eq system-type 'darwin) "open ")
                         ((eq system-type 'gnu/linux) "xdg-open ")
                         ((eq system-type 'windows-nt) "start \"\" "))))
      (dolist (file files)
        (setq command (concat command (shell-quote-argument file) " ")))
      (async-shell-command command))))

(defun tmtxt/dired-open-current-directory ()
  "Open the current directory in Finder."
  (interactive)
  (save-window-excursion
    (let ((command (cond ((eq system-type 'darwin) "open .")
                         ((eq system-type 'gnu/linux) "xdg/open .")
                         ((eq system-type 'windows-nt) "explorer.exe ."))))
      (async-shell-command command))))

(defun tmtxt/dired-extract-zip-at-point ()
  "Extract the .zip file at the current point in Dired."
  (interactive)
  (let ((zip-file (dired-get-file-for-visit)))
    (dired-create-directory (file-name-sans-extension zip-file))
    (async-shell-command (format "tar -xf %s -C %s"
                           (shell-quote-argument zip-file)
                           (shell-quote-argument (file-name-sans-extension zip-file))))
    (revert-buffer)))

(defun tmtxt/copy-pc-name () "" (interactive)
       (kill-new "FD6PB54.wtg.zone"))

(defun insert-windows-pc-name ()
  "Insert the current Windows PC name at point."
  (interactive)
  (let ((pc-name (shell-command-to-string "hostname")))
    (insert pc-name)))

(provide 'tmtxt-dired)
