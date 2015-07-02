;;; Truong Tx's emacs init file

;;; NOTE: the functions with prefix tmtxt- are the functions that I add to
;;; emacs. I just make the prefix to prevent duplicate function if in the future
;;; emacs add the functions with the same name with them

;;; ------------------------------------------------------------------
;;; ------------------------------------------------------------------
;;; define some load path here
;; (add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/config/")

;;; ------------------------------------------------------------------
;;; ------------------------------------------------------------------
;;; temp fix for free variable warning, F**K it
;;; TODO remove this later when the packages on melpa updated
(defvar ido-cr+-enable-next-call nil)
(defvar ido-cr+-replace-completely nil)
(defvar ido-context-switch-command nil)
(defvar ido-ubiquitous-debug-mode nil)

;;; ------------------------------------------------------------------
;;; ------------------------------------------------------------------
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

;;; ------------------------------------------------------------------
;;; ------------------------------------------------------------------
;;; package.el
;;; everytime emacs starts, it will automatically check if those packages are
;;; missing, it will install them automatically
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar tmtxt/elpa-packages
  '(auto-complete                       ;auto complete
    elisp-slime-nav                     ;Make M-. and M-, work in elisp
    find-file-in-project                ;quickly find file in a project
    idle-highlight-mode                 ;highlight the word the point is on
    magit                               ;control git from emacs
    paredit                             ;minor mode for editing parentheses
    popup                               ;visual popup user interface
    yasnippet                           ;yasnippet
    bookmark+                           ;enhance built in bookmark
    markdown-mode                       ;markdown mode for emacs
    textmate                            ;textmate mode
    autopair                            ;auto pair the brackets
    php-mode                            ;php mode for emacs
    ecb                                 ;emacs code browser
                                        ;member-function                     ;expand member functions in C++
    header2                             ;auto file header
    rainbow-mode                        ;colorize color name in buffer
    rainbow-delimiters
    undo-tree                           ;treat undo as a tree
    twittering-mode                     ;twitter client for emacs
    htmlize                             ;generate html from buffer
    puppet-mode                         ;mode for editing puppet files
    yaml-mode                           ;yaml mode
    exec-path-from-shell                ;copy PATH from shell
    thesaurus                           ;thesaurus
    emmet-mode                          ;zen coding style
    web-mode                            ;mode for web
    web-beautify                        ;beautify css, html, js
    ac-ispell                           ;auto complete from ispell
    flycheck                            ;on the fly syntax checking
    expand-region                       ;smart region selection
    smart-forward                       ;smart par forward
    nyan-mode                           ;nyan cat in mode line
    vlf                                 ;view large file
    projectile                          ;find file/folder in project
    auto-complete-clang
    key-chord
    less-css-mode
    sql-indent
    sqlup-mode
    git-messenger
    quack
    highlight-parentheses
    zygospore
    ace-jump-mode
    ;; aggressive-indent
    restclient
    flycheck-pyflakes
    elpy
    jinja2-mode
    elixir-mode
    sass-mode
    org-trello
    dash
    s

    ;; helm
    helm
    ;; helm-swoop
    helm-projectile
    ac-helm
    helm-flycheck
    helm-descbinds

    ;; vim emulation
    evil
    evil-nerd-commenter
    surround
    evil-matchit

    ;; javascript
    json-mode
    js2-mode
    ac-js2
    jsx-mode
    js2-refactor
    react-snippets
    tern
    tern-auto-complete
    html-script-src
    coffee-mode
    nodejs-repl

    ;; ido
    flx                                 ;fuzzy matching
    flx-ido                             ;fuzzy matching for ido
    ido-ubiquitous                      ;use ido nearly everywhere
    smex                                ;M-x interface with ido style
    ido-better-flex                     ;better flex algorithm for ido
    ido-hacks

    ;; clojure
    clojure-mode
    clojure-test-mode
    cider
    ac-cider
    clojure-cheatsheet

    ;; google stuffs
    google-this
    google-translate

    ;; Color theme
    color-theme
    color-theme-solarized

    ;; Dired
    dired-details
    dired-details+
    dired+
    dired-rainbow))
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
(require 'tmtxt-util)					;utilities
(tmtxt/add-lib "single-file-modes")
(require 'tmtxt-misc)					;misc
(require 'tmtxt-ido)					;config for ido
(require 'tmtxt-flymake)				;flymake
(require 'tmtxt-bookmark)				;load the bookmark and my config
(require 'tmtxt-dired)					;dired mode config (for file management)
(require 'tmtxt-navigation)     ;navigation util
(require 'tmtxt-editing)          ;editing config
(require 'tmtxt-auto-complete)          ;
(require 'tmtxt-project)
;; ;; (require 'tmtxt-ecb)					;emacs code browser
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

;;; emacs neo4j shell
(require 'cypher-mode)
(defvar n4s-cli-program "neo4j-shell"
  "The cli program to start neo4j shell")

(defvar n4s-cli-arguments '()
  "List of command line arguments to pass to neo4j shell cli programm")

(defvar n4s-font-lock-keywords cypher-font-lock-keywords
  "Font lock keywords list, default is to taken from cypher-mode")

(defvar n4s-pop-to-buffer nil
  "Whether to pop up the neo4j shell buffer after sending command to execute")

(defvar n4s-pop-to-buffer-function 'pop-to-buffer
  "The function to pop up the neo4j shell buffer")

(define-derived-mode neo4j-shell-mode comint-mode "Neo4j Shell"
  "Major mode for `n4s-start'."
  ;; not allow the prompt to be deleted
  (setq comint-prompt-read-only t)
  ;; font lock keywords
  (set (make-local-variable 'font-lock-defaults) '(n4s-font-lock-keywords t)))

(defun n4s-pop-to-buffer ()
  "Pop the neo4j shell buffer to the current window"
  (apply n4s-pop-to-buffer-function '("*neo4j-shell*")))

;;; Taken from masteringemacs with some changes
;;; https://www.masteringemacs.org/article/comint-writing-command-interpreter
(defun n4s-start ()
  "Start neo4j shell comint mode"
  (interactive)
  (let ((buffer (comint-check-proc "*neo4j-shell*")))
    ;; pop to the "*neo4j-shell*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'neo4j-shell-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create "*neo4j-shell*")
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "neo4j-shell" nil n4s-cli-program nil
             n4s-cli-arguments)
      (neo4j-shell-mode))))

;;; Send the query string to neo4j shell to execute
(defun n4s-send-string (string)
  "Send the input string to neo4j shell process"
  (if (not (comint-check-proc "*neo4j-shell*"))
      (message "No neo4j shell process started")
    (progn
      (process-send-string "*neo4j-shell*" (concat string "\n"))
      (when n4s-pop-to-buffer
        (n4s-pop-to-buffer)))))

(defun n4s-send-region (beg end)
  "Send the region from beg to end to neo4j process"
  (let ((string (buffer-substring-no-properties beg end)))
    (n4s-send-string string)))

(defun n4s-send-current-region ()
  "Send the selected region to neo4j shell process"
  (interactive)
  (let* ((beg (region-beginning))
         (end (region-end)))
    (n4s-send-region beg end)))

(defun n4s-send-buffer ()
  "Send the current buffer to neo4j shell process"
  (interactive)
  (let* ((beg (point-min))
         (end (point-max)))
    (n4s-send-region beg end)))

(defun n4s-send-paragraph ()
  "Send the current paragraph to neo4j shell process"
  (interactive)
  (let ((beg (save-excursion
               (backward-paragraph)
               (point)))
        (end (save-excursion
               (forward-paragraph)
               (point))))
    (n4s-send-region beg end)))

(defun n4s-send-region-or-buffer ()
  "Send the selected region if presented, otherwise, send the whole buffer"
  (interactive)
  (if (use-region-p)
      (n4s-send-current-region)
    (n4s-send-buffer)))

(defun n4s-send-dwim ()
  "Send the selected region presented, otherwise, send the current paragraph"
  (interactive)
  (if (use-region-p)
      (n4s-send-current-region)
    (n4s-send-paragraph)))

(setq n4s-cli-program "vagrant")
(setq n4s-cli-arguments '("ssh" "-c" "/home/vagrant/neo4j/neo4j-community-2.2.1/bin/neo4j-shell -port 7475"))
(setq n4s-pop-to-buffer t)
