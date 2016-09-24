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
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
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
(tmtxt/add-lib "single-file-modes")

;;; some my own useful config
(require 'tmtxt-util)					;utilities
(require 'tmtxt-misc)					;misc
(require 'tmtxt-ido)					;config for ido
(require 'tmtxt-flymake)				;flymake
(require 'tmtxt-bookmark)				;load the bookmark and my config
(require 'tmtxt-dired)					;dired mode config (for file management)
(require 'tmtxt-navigation)     ;navigation util
(require 'tmtxt-editing)          ;editing config
(require 'tmtxt-auto-complete)          ;
(require 'tmtxt-project)
(require 'tmtxt-google)					;config for google stuffs
(require 'tmtxt-desktop)				;auto save mode
(require 'tmtxt-org)					;org mode config
(require 'tmtxt-appearance)				;how my emacs appears
(require 'tmtxt-cc)						;config for cc-mode
(require 'tmtxt-buffers-management)		;config for managing buffer
(require 'tmtxt-shell)					;config for shell
(require 'tmtxt-javascript)				;config for js development
(require 'tmtxt-web)					;config for web development
(require 'tmtxt-php)					;config for php coding
(require 'tmtxt-helm)					;config for helm
(require 'tmtxt-markdown)				;config for markdown
(require 'tmtxt-twitter)				;config for twitter
(require 'tmtxt-lisp)					;config for working with lisp language
(require 'tmtxt-ruby)					;config for working with ruby
(require 'tmtxt-grep)					;grep find
(require 'tmtxt-evil)
(require 'tmtxt-sql)
(require 'tmtxt-git)
(require 'tmtxt-python)
(require 'tmtxt-key-bindings)			;key bindings

;; Save positions in visited files
(setq-default save-place t)
(require 'saveplace)
(setq save-place-file "~/.emacs.d/.saveplace")

;;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; use spotlight search for locate command in macos
(tmtxt/in '(darwin)
  (setq locate-command "mdfind"))

;;; thesaurus
(require 'thesaurus)
(thesaurus-set-bhl-api-key-from-file "~/BigHugeLabs.apikey.txt")
(define-key global-map (kbd "C-x t") 'thesaurus-choose-synonym-and-replace)

;;; clojure
;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;;; auto save file
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq-default require-final-newline t)

;;; garbage collector
(setq gc-cons-threshold 100000000)

;;; recent file
;; (require 'recentf)
;; (recentf-mode 1)
;; (setq recentf-max-menu-items 100)

;;; view large file
;; (require 'vlf-integrate)

;; (setq debug-on-error t)

;;; n4js
(tmtxt/add-lib "n4js")
(require 'n4js)
(setq n4js-cli-program "vagrant")
(setq n4js-cli-arguments '("ssh" "-c" "/home/vagrant/neo4j/neo4j-community-2.2.1/bin/neo4j-shell -port 7475"))
(setq n4js-pop-to-buffer t)
(add-hook 'neo4j-shell-mode (lambda () (toggle-truncate-lines t)))

(server-start)
