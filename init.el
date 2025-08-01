;;; init.el --- Truong Tx's emacs init file

;;; Commentary:
;;; NOTE: the functions with prefix tmtxt- are the functions that I add to
;;; Emacs.   I just make the prefix to prevent duplicate function if in the future
;;; Emacs adds the functions with the same name with them

;;; Code:
;;; define some load path here
(add-to-list 'load-path "~/.emacs.d/config/")

;;; Emacs is not a package manager, and here we load its package manager!
(require 'package)
(dolist (source '(;; ("marmalade" . "http://marmalade-repo.org/packages/")
                  ("org" . "http://orgmode.org/elpa/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ("melpa-stable" . "http://stable.melpa.org/packages/")
                  ("melpa" . "http://melpa.org/packages/")
                  ("elpy" . "http://jorgenschaefer.github.io/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

;;; Add dir in "lib" folder to load path
(defun tmtxt/add-lib (dir-name)
  "Append dir-name to ~/.emacs.d/lib/ and then add them to \"load-path\".

DIR-NAME directory name inside lib folder"
  (add-to-list 'load-path (concat "~/.emacs.d/lib/" dir-name)))

;;; some my own useful config
(dolist (cfg '(dash
               tmtxt-util
               tmtxt-dired
               tmtxt-navigation
               tmtxt-editing
               tmtxt-project
               ;tmtxt-desktop
               tmtxt-appearance
               tmtxt-cc
               tmtxt-buffers-management
               tmtxt-shell
               ; tmtxt-javascript
               tmtxt-web
               ; tmtxt-php
               tmtxt-helm
               tmtxt-markdown
               tmtxt-lisp
               tmtxt-ruby
               tmtxt-grep
               tmtxt-sql
               tmtxt-git
               tmtxt-go
               tmtxt-python
               tmtxt-yaml
               ;tmtxt-fmgsuite
               tmtxt-key-bindings

               ;; ;; other packages
               saveplace
               restclient
               ))
  (require cfg))

;;; some default config
(setq-default
 save-place t                           ;save positions in visited files
 save-place-file "~/.emacs.d/.saveplace"

 custom-file "~/.emacs.d/custom.el"
 require-final-newline t
 gc-cons-threshold 100000000
 ediff-window-setup-function 'ediff-setup-windows-plain
 oddmuse-directory (concat user-emacs-directory "oddmuse")
 diff-switches "-u"
 imenu-auto-rescan t

 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 )

;;; use spotlight search for locate command in macos
(tmtxt/in '(darwin)
  (setq locate-command "mdfind"))

;;; other config
(defalias 'yes-or-no-p 'y-or-n-p)
(load custom-file)                      ;custom file

(unless (string= (getenv "CODESPACES") "true")
  (server-start))

;;; rest client mode
(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))
(add-hook 'restclient-mode-hook 'auto-complete-mode)

(put 'downcase-region 'disabled nil)
