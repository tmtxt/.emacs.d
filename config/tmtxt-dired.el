;;; this is my config for dired mode
;;; its target is to replace macos as well as other os's default file explorer application

;;; my required packages
(require 'tmtxt-util)

;;; some required packages for dired
(require 'dired+)

;;; add lib/single-files-mode to load-path
(tmtxt/add-lib "single-file-modes")

;;; some minor config
(setq dired-recursive-deletes 'always)	;always recursively delete dir
(setq dired-recursive-copies 'always)	;always recursively copy dir
(add-hook 'dired-mode-hook 'esk-turn-on-hl-line-mode) ;highlight current line
;; (dired "~/")							;open home dir when start
(setq dired-dwim-target t)				;auto guess default dir when copy/move

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
;;; after that bind it to the key s-b
(defun tmtxt/dired-mark-backward ()
  (interactive)
  (call-interactively 'dired-mark)
  (call-interactively 'dired-previous-line)	;remove this line if you want the
										;cursor to stay at the current line
  (call-interactively 'dired-previous-line))
(define-key dired-mode-map (kbd "s-b") 'tmtxt/dired-mark-backward)

;;; Mac OS
;;; open file/marked files by default program in mac
;;; http://blog.nguyenvq.com/2009/12/01/file-management-emacs-dired-to-replace-finder-in-mac-os-x-and-other-os/
(tmtxt/in '(darwin)
  (defun tmtxt/dired-do-shell-mac-open ()
	(interactive)
	(save-window-excursion
	  (dired-do-async-shell-command
	   "open" current-prefix-arg
	   (dired-get-marked-files t current-prefix-arg))))
  (define-key dired-mode-map (kbd "s-o") 'tmtxt/dired-do-shell-mac-open))

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
(tmtxt/in '(darwin)						;MacOS
  (defun tmtxt/dired-do-shell-unmount-device ()
	(interactive)
	(save-window-excursion
	  (dired-do-async-shell-command
	   "diskutil unmount" current-prefix-arg
	   (dired-get-marked-files t current-prefix-arg)))))
(tmtxt/in '(gnu/linux)					;Linux
  (defun tmtxt/dired-do-shell-unmount-device ()
	(interactive)
	(save-window-excursion
	  (dired-do-async-shell-command
	   "umount" current-prefix-arg
	   (dired-get-marked-files t current-prefix-arg)))))
;;; bind it to s-u
(define-key dired-mode-map (kbd "s-u") 'tmtxt/dired-do-shell-unmount-device)

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
;;; bind it to s-O (s-S-o)
(define-key dired-mode-map (kbd "s-O") 'tmtxt/dired-open-current-directory-in-finder)

;;; hide details
(tmtxt/set-up 'dired-details+
  ;; show sym link target
  (setq dired-details-hide-link-targets nil)
  ;; omit (not show) files begining with . and #
  (setq-default dired-omit-mode t
				dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")
  ;; toggle omit mode C-o
  (define-key dired-mode-map (kbd "C-o") 'dired-omit-mode))

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

;;; dired async
(tmtxt/in '(darwin gnu/linux)
  (tmtxt/add-lib "tmtxt-dired-async")
  (tmtxt/set-up 'tmtxt-dired-async
	;;; close the result window after 5 seconds
	(setq-default tmtxt/dired-async-post-process-window-show-time "5")
	;;; set the height for the result window (10 lines)
	(setq-default tmtxt/dired-async-result-window-height 10)
	;;; set the compression level for the zip function (0-9)
	(setq-default tmtxt/dired-async-zip-compression-level "9")
	;; delete method for rsync delete
	(setq-default tmtxt/dired-async-rsync-delete-method "--delete-after")
	;; show the progress when rsync
	(setq-default tmtxt/dired-async-rsync-show-progress t)
	;; show verbosity
	(setq-default tmtxt/dired-async-rsync-show-verbosity t)
	;; use archive mode, to preserver time stamp
	(setq-default tmtxt/dired-async-rsync-archive-mode t)
	;; use compression mode
	(setq-default tmtxt/dired-async-rsync-compress-mode t)
	;;; some key bindings
	(define-key dired-mode-map (kbd "C-c C-r") 'tmtxt/dired-async-rsync)
	(define-key dired-mode-map (kbd "C-c C-a") 'tmtxt/dired-async-rsync-multiple-mark-file)
	(define-key dired-mode-map (kbd "C-c C-e") 'tmtxt/dired-async-rsync-multiple-empty-list)
	(define-key dired-mode-map (kbd "C-c C-d") 'tmtxt/dired-async-rsync-multiple-remove-item)
	(define-key dired-mode-map (kbd "C-c C-v") 'tmtxt/dired-async-rsync-multiple)
	(define-key dired-mode-map (kbd "C-c C-z") 'tmtxt/dired-async-zip)
	(define-key dired-mode-map (kbd "C-c C-u") 'tmtxt/dired-async-unzip)
	(define-key dired-mode-map (kbd "C-c C-t") 'tmtxt/dired-async-rsync-delete)
	(define-key dired-mode-map (kbd "C-c C-k") 'tmtxt/dired-async-kill-all)
	(define-key dired-mode-map (kbd "C-c C-n") 'tmtxt/dired-async-move-all-points-to-end)
	(define-key dired-mode-map (kbd "C-c C-s") 'tmtxt/dired-async-get-files-size))) 


;;; open current directory in terminal
(tmtxt/in '(darwin)

  ;; default terminal application path
  (defvar tmtxt/macos-default-terminal-app-path
	"/Applications/Terminal.app" "The default path to terminal application in MacOS")
  (setq-default tmtxt/macos-default-terminal-app-path "/Volumes/tmtxt/Applications/iTerm.app")

;;; function to open new terminal window at current directory
  (defun tmtxt/open-current-dir-in-terminal ()
	"Open current directory in dired mode in terminal application.
For MacOS only"
	(interactive)

	(shell-command (concat "open -a "
						   (shell-quote-argument tmtxt/macos-default-terminal-app-path)
						   " "
						   (shell-quote-argument (file-truename default-directory)))))

;;; bind a key for it
  (define-key dired-mode-map (kbd "C-c C-o") 'tmtxt/open-current-dir-in-terminal))




;;; custom key bindings for dired mode
(define-key dired-mode-map (kbd "C-S-n") 'dired-create-directory)
(define-key dired-mode-map (kbd "C-S-u") 'dired-up-directory)

;;; finally provide the library
(provide 'tmtxt-dired)

