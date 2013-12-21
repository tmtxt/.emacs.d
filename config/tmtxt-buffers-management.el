;;; config for buffer management

(require 'tmtxt-util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Switch to last buffer
(defun tmtxt/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(tmtxt/in '(darwin)
  (global-set-key (kbd "C-S-<tab>") 'tmtxt/switch-to-last-buffer))
(tmtxt/in '(gnu/linux)
  (global-set-key (kbd "<C-S-iso-lefttab>") 'tmtxt/switch-to-last-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffer managers using ibuffer
(require 'ibuffer)
(setq ibuffer-saved-filter-groups
	  (quote (("default"
			   ("Org" ;; all org-related buffers
				(mode . org-mode))
			   ("Dired" ;; all dired-related buffers
				(mode . dired-mode))
			   ("Mail"
				(or  ;; mail-related buffers
				 (mode . message-mode)
				 (mode . mail-mode)))
			   ("Markdown"
				(or
				 (mode . markdown-mode)))
			   ;; ("truongtx blog"
			   ;;   (filename . "~/truongtx.me blog/")) ;replace with the link to project
			   ("JS Programming"
				(or
				 (mode . js-mode)
				 (mode . js2-mode)
				 (mode . javascript-mode)))
			   ("C Programming"
				(or
				 (mode . c++-mode)
				 (mode . cc-mode)
				 (mode . c-mode)))
			   ("Elisp Programming"
				(or
				 (mode . emacs-lisp-mode)))
			   ("ERC"   (mode . erc-mode))))))
(add-hook 'ibuffer-mode-hook
		  (lambda ()
			(ibuffer-switch-to-saved-filter-groups "default")))
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; display those buffers in the same window, not pop-up a new window
(add-to-list 'same-window-buffer-names "*Help*")
(add-to-list 'same-window-buffer-names "*Apropos*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tmtxt/in '(gnu/linux)
  (global-set-key (kbd "s-k") 'kill-this-buffer))

;;; provide
(provide 'tmtxt-buffers-management)
