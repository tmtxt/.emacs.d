;;; config for cc-mode (C, C++,...)
;;; this file should be loaded after tmtxt-editing.el for auto-complete-clang to
;;; work properly

;;; some required packages
(require 'cc-mode)

;;; some minor config
(setq-default
 ;; offset 4
 c-basic-offset 4

 ;; linux style
 c-default-style "linux")

;;; expand member function for C++
(tmtxt/set-up 'member-function
  (setq mf--source-file-extension "cpp"))

;;; clang, auto complete library for C/C++ programming
;;; this feature should be loaded after auto-complete
;;; require "clang" installed on your computer
;;; on MacOS: built in
;;; on Linux: sudo apt-get update”, “sudo apt-get install clang”
(tmtxt/add-lib "auto-complete-clang")	;add it to load-path
(require 'auto-complete-clang)
;;; bind it to C-S-<return>
(define-key c++-mode-map (kbd "C-S-<return>") 'ac-complete-clang)

;;; setup flymake for cc mode
(tmtxt/setup-flymake-for-mode c-mode-map)
(tmtxt/setup-flymake-for-mode c++-mode-map)

;;; finally provide the library
(provide 'tmtxt-cc)
