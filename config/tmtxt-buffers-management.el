;;; config for buffer management

;;; Switch to last buffer
(defun tmtxt/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

;;; provide
(provide 'tmtxt-buffers-management)
