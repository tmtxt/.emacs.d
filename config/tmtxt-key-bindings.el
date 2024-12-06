(require 'web-mode)
(require 'paredit)
(require 'hideshow)
(require 'projectile)
(require 'helm-projectile)
(require 'flycheck)
(require 'yasnippet)

;;; util functions for defining/undefining keys
;;; should be loaded at the end
(defun tmtxt/define-keys (key-map &rest ps)
  "Define key binding pairs for KEY-MAP."
  (declare (indent 1))
  (let ((i 0))
    (while (< i (length ps))
      (if (= (mod i 2) 0)
          (let ((src (elt ps i))
                (dst (elt ps (1+ i))))
            (define-key key-map
              (read-kbd-macro src) (if (stringp dst)
                                       (read-kbd-macro dst)
                                     dst))))
      (setq i (+ i 2)))))

(defun tmtxt/undefine-keys (key-map keys)
  "Undefine keys for keymap"
  (declare (indent 1))
  (dolist (key keys)
    (define-key key-map (read-kbd-macro key) nil)))

(defmacro tmtxt/keys (package map &rest mappings)
  (declare (indent 2))
  `(eval-after-load ,package
     (quote (progn
              (tmtxt/define-keys ,map
                ,@mappings)
              (message "Updated keymap `%s'" ',map)))))

;;; use Windows key as super key
(setq w32-lwindow-modifier 'super)
(setq w32-rwindow-modifier 'super)

;;; undefine keys
(tmtxt/undefine-keys
    global-map
  '("C-M-j"
    "C-M-l"
    "C-x C-d"
    "s-m"
    "<f2> <f2>"
    "C-\""
    "s-l"
    "C-_"
    "C-q"
    "M-9"
    "M-5"
    "M-a"
    "M-y"
    "M-t"))

(tmtxt/undefine-keys
    paredit-mode-map
  '("C-j" "DEL"))

(tmtxt/undefine-keys
    magit-status-mode-map
  '("<C-tab>"))

(tmtxt/undefine-keys
    magit-process-mode-map
  '("<C-tab>"))

;;; key definitions
(tmtxt/define-keys
    global-map
  "C-+"     'text-scale-increase
  "C-_"     'text-scale-decrease
  ;; "M-%"     'query-replace-regexp
  ;; "C-M-%"   'query-replace
  "C-q C-m" 'helm-imenu
  "M-9"     'magit-status
  "M-;"     'comment-dwim

  "M-a"     'helm-M-x

  ;; editing
  "C-M-\\"  'indent-region
  "C-M-|"   'tmtxt/indent-buffer
  "C-q F"   'tmtxt/find-file-as-root
  "C-m"     'newline-and-indent
  "M-/"     'hippie-expand

  ;; isearch
  "M-y"     'isearch-forward-regexp
  "M-t"     'isearch-backward-regexp

  ;; eshell
  "M-5"   'eshell
  ;; "C-S-e"   'tmtxt/helm-eshell

  ;; buffer managements
  "C-S-<tab>"          'tmtxt/switch-to-last-buffer ;OSX,Windows
  "<C-S-iso-lefttab>"  'tmtxt/switch-to-last-buffer ;Ubuntu
  "<C-tab>"            'switch-to-buffer

  ;; "M-="       'er/expand-region         ;expand region

  "{"     'paredit-open-curly
  "}"     'paredit-close-curly

  "M-P" 'goto-line
  )

(tmtxt/define-keys
    key-translation-map

  ;; ergonomic layout
  "M-i"     "C-p"
  "M-k"     "C-n"
  "M-j"     "C-b"
  "M-l"     "C-f"
  "M-I"     "M-v"
  "M-K"     "C-v"
  "M-L"     "C-e"
  "M-J"     "C-a"
  "M-o"     "M-f"
  "M-u"     "M-b"
  "M-n"     "M-<"
  "M-N"     "M->"
  "M-d"     "C-d"
  "M-f"     "DEL"
  "M-z"     "C-/"
  "M-c"     "M-w"
  ;; "M-w"     "C-w"
  "M-v"     "C-y"
  "M-s"     "C-x C-s"
  "M-U"     "C-M-b"
  "M-O"     "C-M-f"
  ;; "M-a"     "C-x h"
  "M-D"     "M-d"
  "M-F"     "M-DEL"
  "M-W"     "s-&"
  "M-m"     "C-m"
  "M-SPC"   "C-SPC"
  "M-g"     "C-k"
  "C-M-S-s" "C-x #"

  ;; move between windows (up/down/left/right)
  "M-s-i"   "<S-up>"
  "M-s-^"   "<S-up>"
  "M-s-k"   "<S-down>"
  "M-s-j"   "<S-left>"
  "M-s-∆"   "<S-left>"
  "M-s-l"   "<S-right>"
  "M-s-¬"   "<S-right>"
  "M-s-}"   "<S-up>"
  "M-s-˚"   "<S-down>"
  "M-s-˝"   "<S-left>"
  "M-s--"   "<S-right>"
  "M-q"     "C-x o"
  "C-S-b"   "C-x +"
  )

(tmtxt/define-keys yas-minor-mode-map
  "C-S-f"     'yas-expand)

(tmtxt/define-keys
    emacs-lisp-mode-map
  "C-c C-r"  'eval-region)

(tmtxt/define-keys
    read-expression-map
  "TAB" 'lisp-complete-symbol)

(tmtxt/define-keys
    lisp-mode-shared-map
  "RET" 'reindent-then-newline-and-indent)

(tmtxt/define-keys
    dired-mode-map
  "C-S-n"   'dired-create-directory
  "C-S-u"   'dired-up-directory
  "C-o"     'dired-omit-mode
  "M-b" 'subword-backward
  "C-M-b" 'paredit-backward
  )

(tmtxt/define-keys
    dired-mode-map
  "s-o"     'tmtxt/dired-do-shell-open)

(tmtxt/in '(windows-nt)
  (tmtxt/define-keys dired-mode-map
    "s-O" 'tmtxt/dired-open-current-directory))

(tmtxt/define-keys
    hs-minor-mode-map
  "C-c C-h"   'hs-hide-block
  "C-c C-d"   'hs-show-block
  "C-c C-t"   'hs-toggle-hiding
  "C-c C-;"   'hs-hide-all
  "C-c C-'"   'hs-show-all
  "C-c C-l"   'hs-hide-level
  "C-S-r"     'hs-toggle-hiding
  )

(tmtxt/keys 'wdired wdired-mode-map
  "s-o"   'tmtxt/dired-do-shell-open)

(tmtxt/keys 'projectile
    global-map
  "C-x C-d"     'projectile-dired
  "C-x i"       'projectile-invalidate-cache)

(tmtxt/keys 'helm
    global-map
  "C-S-q" 'tmtxt/helm
  "C-S-s"     'helm-occur)

(tmtxt/keys 'helm helm-map
  "C-M-S-k"  'helm-next-source
  "C-M-S-i"  'helm-previous-source)

(tmtxt/keys 'helm-projectile
    global-map

  "C-x C-S-f"        'helm-projectile-find-file
  "C-x C-S-d"        'helm-projectile-find-dir
  "C-x C-S-r"        'helm-projectile-ag)

(tmtxt/keys 'helm-dired-recent-dirs
    global-map
  "C-c C-y"  'helm-dired-recent-dirs-view)

(tmtxt/define-keys js2-mode-map
  "C-M-\"" 'js2-mark-defun
  "C-M-:" 'js2-mode-toggle-hide-functions
  "C-M->" 'js2-mode-toggle-element
  "C-M-|"   'prettier-prettify
  )

(tmtxt/define-keys js2-jsx-mode-map
  "s-j"  'tmtxt/switch-to-web-mode)

(tmtxt/keys 'flycheck
    flycheck-mode-map
  "C-x C-p"     'helm-flycheck)

(tmtxt/define-keys comint-mode-map
  "C-M-i"    'comint-previous-matching-input-from-input
  "C-M-k"    'comint-next-matching-input-from-input)

(tmtxt/define-keys global-map
  "C-c m"   'git-messenger:popup-message)

(tmtxt/define-keys web-mode-map
  "s-y"   'tmtxt/web-mode-change-indentation
  "s-g"   'web-mode-element-content-select
  "s-I"   'web-mode-element-parent
  "s-K"   'web-mode-element-child
  "s-L"   'web-mode-element-next
  "s-J"   'web-mode-element-previous
  "s-h"   'web-mode-element-kill
  "s-n"   'web-mode-element-select
  ;; web mode tag match function not defined as interactive
  "s-m"   (lambda () (interactive) (web-mode-tag-match))
  "s-j"   'tmtxt/switch-to-js2-jsx-mode)

;;; Windows specific keys
(tmtxt/in '(windows-nt)
  (tmtxt/define-keys key-translation-map
    "<M-f4>" "C-x C-c"))

(add-hook
 'eshell-mode-hook
 (lambda ()
   (tmtxt/define-keys eshell-mode-map
     "C-S-i" 'eshell-previous-input
     "C-S-k" 'eshell-next-input
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the rest is taken from starter kit
;; Help should search more than just commands
(define-key 'help-command "a" 'apropos)

;; This is a little hacky since VC doesn't support git add internally
(eval-after-load 'vc
  (define-key vc-prefix-map "i"
    '(lambda () (interactive)
       (if (not (eq 'Git (vc-backend buffer-file-name)))
           (vc-register)
         (shell-command (format "git add %s" buffer-file-name))
         (message "Staged changes.")))))

(provide 'tmtxt-key-bindings)
