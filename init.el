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
										;theydo in slime
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
	;; helm								;extend to enything.el
	rainbow-mode						;colorize color name in buffer
	undo-tree							;treat undo as a tree
	twittering-mode						;twitter client for emacs
	htmlize								;generate html from buffer
	puppet-mode							;mode for editing puppet files
	ergoemacs-mode
	yaml-mode
	exec-path-from-shell
	
	;; js2 mode
	;; js2-mode ac-js2

	;; ido
	ido-ubiquitous						;use ido nearly everywhere
	smex								;M-x interface with ido style
	ido-better-flex						;better flex algorithm for ido

	;; clojure
	clojure-mode clojure-test-mode nrepl

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
(require 'tmtxt-textmate)				;config for textmate mode
(require 'tmtxt-appearance)				;how my emacs appears
(require 'tmtxt-markup)					;config for markup languages
(require 'tmtxt-php)					;config for php coding
(require 'tmtxt-cc)						;config for cc-mode
(require 'tmtxt-buffer)					;config for managing buffer
(require 'tmtxt-emmet)					;config for emmet mode
(require 'tmtxt-shell)
(require 'tmtxt-html)					;config for html mode
;; (require 'tmtxt-webmode)
;; (require 'tmtxt-helm)					;config for helm
;;; add lib/single-file-modes to load-path
(tmtxt/add-lib "single-file-modes")

;;; ------------------------------------------------------------------
;;; ------------------------------------------------------------------
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
				 (mode . mail-mode)
				 ;; etc.; all your mail related modes
				 ))
			   ;; ("MyProject"
			   ;;   (filename . "src/myproject/")) ;replace with the link to project
			   ("Programming" ;; prog stuff not already in MyProjectX
				(or
				 (mode . perl-mode)
				 (mode . python-mode)
				 ;; etc
				 ))
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

;;; ------------------------------------------------------------------
;;; ------------------------------------------------------------------
;;; markdown mode for emacs
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
;;; associate markdown with .md file
(setq auto-mode-alist
	  (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
	  (cons '("\\.markdown" . markdown-mode) auto-mode-alist))

;;; ------------------------------------------------------------------
;;; ------------------------------------------------------------------
;; Save positions in visited files
(setq-default save-place t)
(require 'saveplace)
(setq save-place-file "~/.emacs.d/.saveplace")

;;; ------------------------------------------------------------------
;;; ------------------------------------------------------------------
;;; twittering mode
(require 'twittering-mode)
(setq twittering-status-format
	  "%i %s, %FACE[ublt-twitter-meta-face]{%@ from %f%L%r%R}\n\t%t"
	  twittering-url-show-status nil
	  twittering-timer-interval 600
	  twittering-use-master-password t
	  twittering-use-icon-storage t
	  twittering-display-remaining t
	  twittering-number-of-tweets-on-retrieval 100
	  twittering-icon-mode t)
(add-hook 'twittering-mode-hook (tmtxt/on-fn 'hl-line-mode))
(define-key twittering-edit-mode-map (kbd "C-c C-t")
  'twittering-tinyurl-replace-at-point)

;;; ------------------------------------------------------------------
;;; ------------------------------------------------------------------
;;; other config

;;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; bind (compile) to C-x c
(global-set-key (kbd "C-x c") 'compile)

;;; eval region to C-c C-r
(define-key lisp-mode-map (kbd "C-c C-r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
;; (when (file-executable-p "~/bin/macports/bin/gpg") (setq epg-gpg-program "~/bin/macports/bin/gpg"))

;; (setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
;; (setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
(ergoemacs-mode 0)

;;; use spotlight search for locate command in macos
(tmtxt/in '(darwin)
  (setq locate-command "mdfind"))

;;; magit git executable file
;; (when (file-executable-p "~/bin/macports/bin/git")
;;   (setq magit-git-executable "~/bin/macports/bin/git"))

;;; moz minor mode
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))
