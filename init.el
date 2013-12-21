;;; Truong Tx's emacs init file

;;; NOTE: the functions with prefix tmtxt- are the functions that I add to
;;; emacs. I just make the prefix to prevent duplicate function if in the future
;;; emacs add the functions with the same name with them

;;; ------------------------------------------------------------------
;;; ------------------------------------------------------------------
;;; define some load path here
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/config/")

;;; ------------------------------------------------------------------
;;; ------------------------------------------------------------------
;;; Emacs is not a package manager, and here we load its package manager!
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ;; TODO: Maybe, use this after emacs24 is released
                  ;; (development versions of packages)
                  ("melpa" . "http://melpa.milkbox.net/packages/")
                  ))
  (add-to-list 'package-archives source t))
(package-initialize)

;;; ------------------------------------------------------------------
;;; ------------------------------------------------------------------
;;; package.el
;;; everytime emacs starts, it will automatically check if those packages are
;;; missing, it will install them automatically
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar tmtxt/elpa-packages
  '(auto-complete						;auto complete
	elisp-slime-nav						;Make M-. and M-, work in elisp like
										;they do in slime
	find-file-in-project				;quickly find file in a project
	idle-highlight-mode					;highlight the word the point is on
	magit								;control git from emacs
	paredit								;minor mode for editing parentheses
	popup								;visual popup user interface
	yasnippet							;yasnippet
	bookmark+							;enhance built in bookmark
	markdown-mode						;markdown mode for emacs
	textmate							;textmate mode
	autopair							;auto pair the brackets
	php-mode							;php mode for emacs
	ecb									;emacs code browser
	member-function						;expand member functions in C++
	header2								;auto file header
	rainbow-mode						;colorize color name in buffer
	undo-tree							;treat undo as a tree
	twittering-mode						;twitter client for emacs
	htmlize								;generate html from buffer
	puppet-mode							;mode for editing puppet files
	ergoemacs-mode
	yaml-mode
	exec-path-from-shell
	thesaurus
	emmet-mode
	web-mode

	;; helm
	helm helm-swoop

	;; vim emulation
	evil surround
	
	;; javascript
	js2-mode ac-js2

	;; ido
	ido-ubiquitous						;use ido nearly everywhere
	smex								;M-x interface with ido style
	ido-better-flex						;better flex algorithm for ido

	;; clojure
	clojure-mode cider

	;; google stuffs
	google-this google-translate

	;; starter kit
	starter-kit starter-kit-bindings starter-kit-eshell starter-kit-js
	starter-kit-lisp starter-kit-perl starter-kit-ruby

	;; Color theme
	color-theme color-theme-solarized

	;; Dired
	dired-details dired-details+ dired+))
(dolist (p tmtxt/elpa-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; ------------------------------------------------------------------
;;; ------------------------------------------------------------------
;; ;;; el-get, another package manager for emacs
;; ;;; init it
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))
;; ;;; auto install missing packages
;; ;;; everytime emacs starts, it will check for those packages, if they are not
;; ;;; installed, auto install them
;; (defvar tmtxt/el-get-packages
;;   '(wanderlust							;email client for emacs
;; 	o-blog								;blog for org-mode
;; 	))
;; (dolist (p tmtxt/el-get-packages)
;;   (when (not (el-get-package-exists-p p))
;; 	(el-get-install p)))
;; ;;; sync packages
;; (el-get 'sync)

;;; ------------------------------------------------------------------
;;; ------------------------------------------------------------------
;;; some my own useful config
(require 'tmtxt-util)
(tmtxt/add-lib "single-file-modes")
(require 'tmtxt-ido)					;config for ido
(require 'tmtxt-flymake)				;flymake
(require 'tmtxt-bookmark)				;load the bookmark and my config
(require 'tmtxt-dired)					;dired mode config (for file management)
(require 'tmtxt-navigation)				;navigation util
(require 'tmtxt-editing)				;editing config
(require 'tmtxt-ecb)					;emacs code browser
(require 'tmtxt-google)					;config for google stuffs
(require 'tmtxt-desktop)				;auto save mode
(require 'tmtxt-org)					;org mode config
(require 'tmtxt-appearance)				;how my emacs appears
(require 'tmtxt-php)					;config for php coding
(require 'tmtxt-cc)						;config for cc-mode
(require 'tmtxt-buffers-management)		;config for managing buffer
(require 'tmtxt-shell)					;config for shell
(require 'tmtxt-javascript)				;config for js development
(require 'tmtxt-web)					;config for web development
(require 'tmtxt-helm)					;config for helm
(require 'tmtxt-markdown)				;config for markdown
(require 'tmtxt-twitter)				;config for twitter

;; Save positions in visited files
(setq-default save-place t)
(require 'saveplace)
(setq save-place-file "~/.emacs.d/.saveplace")

;;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; bind (compile) to C-x c
(global-set-key (kbd "C-x c") 'compile)

;;; eval region to C-c C-r
(define-key lisp-mode-map (kbd "C-c C-r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)

;;; use spotlight search for locate command in macos
(tmtxt/in '(darwin)
  (setq locate-command "mdfind"))

;;; thesaurus
(require 'thesaurus)
(thesaurus-set-bhl-api-key-from-file "~/BigHugeLabs.apikey.txt")
(define-key global-map (kbd "C-x t") 'thesaurus-choose-synonym-and-replace)

;;; clojure
;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;;; autocomplete from ispell
(custom-set-variables
 '(ac-ispell-requires 4))
(eval-after-load "auto-complete"
  '(progn
	 (ac-ispell-setup)))
(defun my/enable-ac-ispell ()
  (add-to-list 'ac-sources 'ac-source-ispell))
(add-hook 'markdown-mode-hook 'my/enable-ac-ispell)
