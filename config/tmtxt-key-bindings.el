(require 'web-mode)
(require 'paredit)
(require 'hideshow)
(require 'helm)
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
(tmtxt/undefine-keys global-map
  '("C-M-j" "C-M-l" "C-x C-d" "s-m" "<f2> <f2>" "C-\"" "s-l" "C-_" "C-q"
    "M-a" "C-z" "M-x"))

(tmtxt/undefine-keys paredit-mode-map '("C-j" "DEL"))
(tmtxt/undefine-keys magit-status-mode-map '("<C-tab>"))
(tmtxt/undefine-keys magit-process-mode-map '("<C-tab>"))

;;; global keys
(tmtxt/define-keys
    global-map
  "C-+"     'text-scale-increase
  "C-_"     'text-scale-decrease
  ;; "M-%"     'query-replace-regexp
  ;; "C-M-%"   'query-replace
  "C-q C-m" 'helm-imenu
  "C-S-q"   'tmtxt/helm
  "C-Y"     'helm-occur
  "M-9"     'magit-status
  "M-;"     'comment-dwim
  "M-a"     'helm-M-x
  "C-M-\\"  'indent-region
  "C-M-|"   'tmtxt/indent-buffer
  "C-q F"   'tmtxt/find-file-as-root
  "C-m"     'newline-and-indent
  "M-/"     'hippie-expand
  "M-5"     'eshell
  "<C-tab>"            'switch-to-buffer
  "{"     'paredit-open-curly
  "}"     'paredit-close-curly
  "("     'paredit-open-round
  ")"     'paredit-close-round
  "["     'paredit-open-square
  "]"     'paredit-close-square
  "\""    'paredit-doublequote
  "M-P" 'goto-line
  "C-q i"            'projectile-invalidate-cache
  "C-q C-S-f"        'helm-projectile-find-file
  "C-q C-S-d"        'helm-projectile-find-dir
  "C-q C-d"          'projectile-dired
  )
(tmtxt/define-keys key-translation-map
  "M-U"     "C-M-b"
  "M-O"     "C-M-f"
  "M-y"     "C-s"
  "M-t"     "C-r"
  "M-m"     "C-m"
  "M-SPC"   "C-SPC"
  "C-q C-j" "C-x C-j"
  "C-S-j"   "<S-left>"
  "C-S-l"   "<S-right>"
  "C-S-i"   "<S-up>"
  "C-S-k"   "<S-down>"
  "C-q C-q" "C-x C-x")

;;; on windows, no default key for kill-current-buffer so cannot use translation-map
(tmtxt/in '(windows-nt)
  (tmtxt/define-keys global-map
    "M-w"                'kill-current-buffer
    "M-c"                'kill-ring-save
    "C-S-<tab>"          'tmtxt/switch-to-last-buffer
    "M-Y"              'helm-grep-do-git-grep)
  (tmtxt/define-keys key-translation-map
    "C-z"              "C-/"
    "M-x"              "C-w"
    "M-s"              "C-x C-s"
    "M-v"              "C-y"))
;;; on mac, there is a default key s-& so we can simply use translation-map
(tmtxt/in '(darwin)
  (tmtxt/define-keys key-translation-map
    "M-c"              "M-w"
    "M-w"              "s-&"
    "s-<up>"           "M-<"
    "s-<down>"         "M->"
    "M-<delete>"       "M-d")
  (tmtxt/define-keys global-map
    "C-S-<tab>"          'tmtxt/switch-to-last-buffer
    "M-Y"              'helm-projectile-ag))
(tmtxt/in '(gnu/linux)
  (tmtxt/define-keys global-map
    "<C-S-iso-lefttab>"  'tmtxt/switch-to-last-buffer))

(tmtxt/define-keys yas-minor-mode-map
  "C-S-f"     'yas-expand)

(tmtxt/define-keys read-expression-map
  "TAB" 'lisp-complete-symbol)

(tmtxt/define-keys lisp-mode-shared-map
  "RET" 'reindent-then-newline-and-indent)

(tmtxt/define-keys dired-mode-map
  "C-S-n"   'dired-create-directory
  "C-S-u"   'dired-up-directory
  "C-o"     'dired-omit-mode
  "s-O"     'tmtxt/dired-do-shell-open
  "s-D"     'tmtxt/dired-open-current-directory)

(tmtxt/define-keys hs-minor-mode-map
  "M--"   'hs-hide-block
  "M-="   'hs-show-block
  "M-_"   'hs-hide-all
  "M-+"   'hs-show-all)

(tmtxt/define-keys helm-map
  "C-M-S-k"  'helm-next-source
  "C-M-S-i"  'helm-previous-source)

;; (tmtxt/keys 'helm-dired-recent-dirs
;;     global-map
;;   "C-c C-y"  'helm-dired-recent-dirs-view)

;; (tmtxt/define-keys js2-mode-map
;;   "C-M-\"" 'js2-mark-defun
;;   "C-M-:" 'js2-mode-toggle-hide-functions
;;   "C-M->" 'js2-mode-toggle-element
;;   "C-M-|"   'prettier-prettify
;;   )

;; (tmtxt/define-keys js2-jsx-mode-map
;;   "s-j"  'tmtxt/switch-to-web-mode)

;; (tmtxt/keys 'flycheck
;;     flycheck-mode-map
;;   "C-x C-p"     'helm-flycheck)

;; (tmtxt/define-keys comint-mode-map
;;   "C-M-i"    'comint-previous-matching-input-from-input
;;   "C-M-k"    'comint-next-matching-input-from-input)

;; (tmtxt/define-keys web-mode-map
;;   "s-y"   'tmtxt/web-mode-change-indentation
;;   "s-g"   'web-mode-element-content-select
;;   "s-I"   'web-mode-element-parent
;;   "s-K"   'web-mode-element-child
;;   "s-L"   'web-mode-element-next
;;   "s-J"   'web-mode-element-previous
;;   "s-h"   'web-mode-element-kill
;;   "s-n"   'web-mode-element-select
;;   ;; web mode tag match function not defined as interactive
;;   "s-m"   (lambda () (interactive) (web-mode-tag-match))
;;   "s-j"   'tmtxt/switch-to-js2-jsx-mode)

;;; Windows specific keys
(tmtxt/in '(windows-nt)
  (tmtxt/define-keys key-translation-map
    "<M-f4>" "C-x C-c"))

;; (add-hook
;;  'eshell-mode-hook
;;  (lambda ()
;;    (tmtxt/define-keys eshell-mode-map
;;      "C-S-i" 'eshell-previous-input
;;      "C-S-k" 'eshell-next-input
;;      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the rest is taken from starter kit
;; Help should search more than just commands
(define-key 'help-command "a" 'apropos)

;; This is a little hacky since VC doesn't support git add internally
;; (eval-after-load 'vc
;;   (define-key vc-prefix-map "i"
;;     '(lambda () (interactive)
;;        (if (not (eq 'Git (vc-backend buffer-file-name)))
;;            (vc-register)
;;          (shell-command (format "git add %s" buffer-file-name))
;;          (message "Staged changes.")))))

(provide 'tmtxt-key-bindings)
