;;; this is my config for dired mode
;;; its target is to replace macos as well as other os's default file explorer application

;;; my required packages
(require 'tmtxt-util)

;;; Dired Omit Mode
;; omit (not show) files begining with . and #
(setq-default dired-omit-mode t
              dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")

;; delete *.tp from omit mode
(setq-default dired-omit-extensions (remove ".tp" dired-omit-extensions))

;;; some required packages for dired
(require 'dired+)

;;; add lib/single-files-mode to load-path
(tmtxt/add-lib "single-file-modes")

;;; some minor config
(setq dired-recursive-deletes 'always)  ;always recursively delete dir
(setq dired-recursive-copies 'always) ;always recursively copy dir
;; (dired "~/")             ;open home dir when start
(setq dired-dwim-target t)        ;auto guess default dir when copy/move

;;; delete files by moving to the folder emacs in Trash folder
;;; this path is for MacOS users
;;; for other os, just set the correct path of the trash
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;;; mark file and then move the cursor back
;;; different from the built in dired-mark
;;; dired-mark marks a file and then move the cursor to the next file
;;; tmtxt-dired-mark-backward marks a file but then move the cursor to the
;;; previous file
(defun tmtxt/dired-mark-backward ()
  (interactive)
  (call-interactively 'dired-mark)
  (call-interactively 'dired-previous-line) ;remove this line if you want the
                                        ;cursor to stay at the current line
  (call-interactively 'dired-previous-line))

;;; Mac OS
;;; open file/marked files by default program in mac
;;; http://blog.nguyenvq.com/2009/12/01/file-management-emacs-dired-to-replace-finder-in-mac-os-x-and-other-os/
(tmtxt/in '(darwin)
  (defun tmtxt/dired-do-shell-mac-open ()
    (interactive)
    (save-window-excursion
      (let ((files (dired-get-marked-files nil current-prefix-arg))
            command)
        ;; the open command
        (setq command "open ")
        (dolist (file files)
          (setq command (concat command (shell-quote-argument file) " ")))
        (message command)
        ;; execute the command
        (async-shell-command command)))))

;;; this is for MacOS only
;;; Show the Finder's Get Info window
;;; it use the script GetInfoMacOS in the MacOS directory in .emacs.d
;;; currently it can only show the info dialog for 1 file
(tmtxt/in '(darwin)
  (defun tmtxt/dired-do-shell-mac-get-info ()
    (interactive)
    (save-window-excursion
      (dired-do-async-shell-command
       "~/.emacs.d/MacOS/GetInfoMacOS" current-prefix-arg
       (dired-get-marked-files t current-prefix-arg)))))

;; unmount disk in dired
;;http://loopkid.net/articles/2008/06/27/force-unmount-on-mac-os-x
(tmtxt/in '(darwin)           ;MacOS
  (defun tmtxt/dired-do-shell-unmount-device ()
    (interactive)
    (save-window-excursion
      (dired-do-async-shell-command
       "diskutil unmount" current-prefix-arg
       (dired-get-marked-files t current-prefix-arg)))))
(tmtxt/in '(gnu/linux)          ;Linux
  (defun tmtxt/dired-do-shell-unmount-device ()
    (interactive)
    (save-window-excursion
      (dired-do-async-shell-command
       "umount" current-prefix-arg
       (dired-get-marked-files t current-prefix-arg)))))

;;; open current directory in Finder (MacOSX)
;;; can apply to other buffer type (not only dired)
;;; in that case, calling this function will cause Finder to open the directory
;;; that contains the current open file in that buffer
(tmtxt/in '(darwin)
  (defun tmtxt/dired-open-current-directory-in-finder ()
    "Open the current directory in Finder"
    (interactive)
    (save-window-excursion
      (async-shell-command
       "open ."))))

;;; hide details
(tmtxt/set-up 'dired-details+
  ;; show sym link target
  (setq dired-details-hide-link-targets nil))

;;; directory first by default
;;; on Mac OS, first install coreutils and findutils, which are the gnu version
;;; of some shell program including ls
;;; sudo port install coreutils findutils
;;; (optional) add this to .bashrc or .zshrc file for them to run in shell
;;; export PATH=/opt/local/libexec/gnubin:$PATH
;;; on ubuntu, no need to do so since it's ship with gnu version ones
;; (tmtxt/in '(darwin)
;;   (require 'ls-lisp)
;;   (setq ls-lisp-use-insert-directory-program t)
;;   (setq insert-directory-program "~/bin/macports/libexec/gnubin/ls"))
(tmtxt/set-up 'dired-sort-map
  (setq dired-listing-switches "--group-directories-first -alh"))

(tmtxt/in '(darwin gnu/linux)
  (tmtxt/add-lib "tmtxt-async-tasks")
  (tmtxt/set-up 'tmtxt-async-tasks))

;;; dired async
(tmtxt/in '(darwin gnu/linux)
  (tmtxt/add-lib "tmtxt-dired-async")
  (tmtxt/set-up 'tmtxt-dired-async
    ;; get file size command
    (setq tda/get-files-size-command "du")
    ;; download command
    (setq tda/download-command "wget")))

;;; open current directory in terminal
(tmtxt/in '(darwin)

  ;; default terminal application path
  (defvar tmtxt/macos-default-terminal-app-path
    "/Applications/Terminal.app" "The default path to terminal application in MacOS")
  (setq-default tmtxt/macos-default-terminal-app-path "/Volumes/tmtxt/Applications/iTerm.app")

  ;; function to open new terminal window at current directory
  (defun tmtxt/open-current-dir-in-terminal ()
    "Open current directory in dired mode in terminal application.
For MacOS only"
    (interactive)

    (shell-command (concat "open -a "
                           (shell-quote-argument tmtxt/macos-default-terminal-app-path)
                           " "
                           (shell-quote-argument (file-truename default-directory))))))

;;; fast renaming for wdired
(tmtxt/set-up 'wdired
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
    (tmtxt/mark-file-name-for-rename)))

;;; finally provide the library
(provide 'tmtxt-dired)
