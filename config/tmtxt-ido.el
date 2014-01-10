;;; config for ido

;;; some require packages
(require 'tmtxt-util)
(require 'ido)
(require 'ido-ubiquitous)
(require 'smex)

;;; enable ido and ido ubiquitous
(ido-mode t)
(ido-ubiquitous t)

;;; smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;;; Set buffer separator in the mini buffer when press C-x b (ido-switch-buffer)
;;; to new line instead of the character | so that it can be easy to read
(setq ido-decorations
	  '("\n=> " "" "\n" "" "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))

;;; some config
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

;;; C-S-b for buffer switching
(global-set-key (kbd "<C-tab>") 'ido-switch-buffer)

;;; load ido hacks (only if in emacs 24 and above)
;;; for emacs 23 and lower, use this
;;; http://www0.fh-trier.de/~politza/emacs/ido-hacks.el.gz
(when (>= emacs-major-version 24)
  (tmtxt/add-lib "ido-hacks")
  (require 'ido-hacks))

;;; finally, provide the library
(provide 'tmtxt-ido)
