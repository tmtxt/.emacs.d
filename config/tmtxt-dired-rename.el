;;; rename features for dired

(defun tmtxt/wdired-mark-file-name-interactive ()
  "Mark file name on the current line except its extension"
  (interactive)
  (let ((current-file-name
		 (dired-get-filename 'verbatim)))
	(let ((current-file-extension
		   (file-name-extension current-file-name)))

	  (activate-mark)
	  (end-of-line)
	  (set-window-point
	   (selected-window)
	   (- (point) (length current-file-extension)))
	  ;; (backward-char (+ (length current-file-extension) 1))
	  (push-mark
	   (- (point) (length current-file-name))
	   t t)
	  (set-mark-command)

	  )))

(defun tmtxt/wdired-mark-file-name ()
  "Mark file name on the current line in wdired mode"
  (call-interactively 'tmtxt/wdired-mark-file-name-interactive))



(provide 'tmtxt-dired-rename)
