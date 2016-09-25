;;; config for desktop session mode

(require 'desktop)

;;; enable
(desktop-save-mode 1)

;;; config
(setq-default
 history-length 100                     ;maximum elements to be saved
 )

;;; auto override state locks
;; http://www.emacswiki.org/DeskTop
(defun tmtxt/emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
      (when (file-exists-p cmdline-file)
        (with-temp-buffer
          (insert-file-contents-literally cmdline-file)
          (goto-char (point-min))
          (search-forward "emacs" nil t)
          pid)))))
(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (tmtxt/emacs-process-p ad-return-value))
    (setq ad-return-value nil)))

;;; specifiy buffers not to be saved
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'sr-mode)
(add-to-list 'desktop-modes-not-to-save 'sr-tree-mode)

;;; finally, provide the library
(provide 'tmtxt-desktop)
