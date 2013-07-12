;;; config for ido

;;; some require packages
(require 'tmtxt-util)
(require 'ido)							;in newer emacs, it's not neccessary

;;; Set buffer separator in the mini buffer when press C-x b (ido-switch-buffer)
;;; to new line instead of the character | so that it can be easy to read
(setq ido-decorations '("\n=> " "" "\n" "" "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))

;;; C-S-b for buffer switching
(global-set-key (kbd "C-S-b") 'ido-switch-buffer)

;;; load ido hacks (only if in emacs 24 and above)
;;; for emacs 23 and lower, use this
;;; http://www0.fh-trier.de/~politza/emacs/ido-hacks.el.gz
(when (>= emacs-major-version 24)
  (tmtxt/add-lib "ido-hacks")
  (require 'ido-hacks))

;;;
(global-unset-key (kbd "C-x <right>"))
(global-set-key (kbd "C-x <right>") 'ido-find-file)

;;; finally, provide the library
(provide 'tmtxt-ido)
