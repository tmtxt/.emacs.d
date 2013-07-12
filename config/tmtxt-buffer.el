;;; config for buffer management

(require 'tmtxt-util)

;;; Switch to last buffer
(defun tmtxt/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(tmtxt/in '(darwin)
  (global-set-key (kbd "C-S-<tab>") 'tmtxt/switch-to-last-buffer))
(tmtxt/in '(gnu/linux)
  (global-set-key (kbd "<C-S-iso-lefttab>") 'tmtxt/switch-to-last-buffer))

;;; display those buffers in the same window, not pop-up a new window
(add-to-list 'same-window-buffer-names "*Help*")

(tmtxt/in '(gnu/linux)
  (global-set-key (kbd "s-k") 'kill-this-buffer))

;;; provide
(provide 'tmtxt-buffer)
