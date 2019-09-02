(require 'web-mode)
(require 'paredit)
(require 'hideshow)
(require 'projectile)
(require 'helm-projectile)
(require 'flycheck)
(require 'smart-forward)
(require 'sql-indent)
(require 'git-messenger)
(require 'ace-jump-mode)
(require 'cider)
(require 'cypher-mode)
(require 'yasnippet)
(require 'golden-ratio)

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

;;; key definitions
(tmtxt/undefine-keys
    global-map
  '("C-M-j" "C-M-l" "C-x C-d" "s-m" "<f2> <f2>" "C-\"" "s-l"))

(tmtxt/undefine-keys
    paredit-mode-map
  '("C-j"))

(tmtxt/define-keys
    global-map
  ;; "C-c f"   'find-file-in-project
  "C-+"     'text-scale-increase
  "C--"     'text-scale-decrease
  "M-%"     'query-replace-regexp
  "C-M-%"   'query-replace
  "C-x C-i" 'helm-imenu
  ;; "C-c y"   'bury-buffer
  ;; "C-c r"   'revert-buffer
  "C-c g"   'magit-status
  ;; "C-x c"   'compile
  "M-;"     'comment-dwim
  "M-:"     'evilnc-comment-or-uncomment-lines
  "s-&"     'kill-this-buffer
  "M-t"     'tmtxt/insert-tab-as-spaces

  "M-x"     'helm-M-x
  "C-M-\""  'mark-defun

  ;; editing
  "C-c q"   'join-line
  ;; "C-c C-a" 'tmtxt/select-all-line
  "C-S-a"   'tmtxt/select-all-line
  "C-M-\\"  'tmtxt/indent-region-or-buffer
  "C-x F"   'tmtxt/find-file-as-root
  ;; "C-S-j"   'textmate-shift-left
  ;; "C-S-l"   'textmate-shift-right
  "C-m"     'newline-and-indent
  "C-M-u"   'upcase-word
  "C-M-j"   'downcase-word
  "M-/"     'hippie-expand
  "s-y"     'tmtxt/change-indentation-locally
  ;; "M-G"     'kill-whole-line

  ;; isearch
  "C-s"     'isearch-forward-regexp
  "\C-r"    'isearch-backward-regexp
  "C-M-s"   'isearch-forward
  "C-M-r"   'isearch-backward

  ;; eshell
  "C-x m"   'tmtxt/eshell
  "C-x M"   (lambda () (interactive) (eshell t)) ;Start a new eshell even if one is active.

  ;; org mode
  "C-c l"   'org-store-link
  "C-c a"   'org-agenda
  "C-c b"   'org-iswitchb

  ;; buffer managements
  "C-S-<tab>"          'tmtxt/switch-to-last-buffer ;OSX,Windows
  "<C-S-iso-lefttab>"  'tmtxt/switch-to-last-buffer ;Ubuntu
  "C-x C-b"            'ibuffer
  "<C-tab>"            'switch-to-buffer

  "M-="       'er/expand-region         ;expand region
  ;; "C-M-S-s"   'tmtxt/sql-connect-server
  ;; "C-M-S-c"   'tmtxt/switch-to-cider-repl

  ;; windows related keys
  ;; "M-s-1"     'golden-ratio-mode
  ;; "C-M-S-w"  'eyebrowse-close-window-config

  "{"     'paredit-open-curly
  "}"     'paredit-close-curly

  "C-S-p" 'goto-line
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
  "M-w"     "C-w"
  "M-v"     "C-y"
  "M-s"     "C-x C-s"
  "M-U"     "C-M-b"
  "M-O"     "C-M-f"
  "M-a"     "C-x h"
  "M-D"     "M-d"
  "M-F"     "M-DEL"
  "M-W"     "s-&"
  "M-m"     "C-m"
  "M-SPC"   "C-SPC"
  "M-g"     "C-k"
  "C-M-S-s" "C-x #"

  ;; move between windows (up/down/left/right)
  "M-s-i"   "<S-up>"
  "M-s-k"   "<S-down>"
  "M-s-j"   "<S-left>"
  "M-s-l"   "<S-right>"
  "M-s-}"   "<S-up>"
  "M-s-˚"   "<S-down>"
  "M-s-˝"   "<S-left>"
  "M-s--"   "<S-right>"
  "M-q"     "C-x o"
  "M-s-o"   "C-x o"
  "C-S-b"   "C-x +"
  ;; "s-d"     "C-x 3"
  ;; "s-D"     "C-x 2"
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
    org-mode-map

  ;; move meta up/down/left/right
  "C-s-j"       'org-metaleft
  "C-s-l"       'org-metaright
  "C-s-i"       'org-metaup
  "C-s-k"       'org-metadown

  ;; shift meta up/down/left/right
  "C-S-s-j"       'org-shiftmetaleft
  "C-S-s-l"       'org-shiftmetaright
  "C-S-s-i"       'org-shiftmetaup
  "C-S-s-k"       'org-shiftmetadown

  ;; navigation between headings
  "C-M-S-k"       'outline-next-visible-heading
  "C-M-S-i"       'outline-previous-visible-heading
  "C-M-S-l"       'org-forward-heading-same-level
  "C-M-S-j"       'org-backward-heading-same-level
  "C-M-S-u"       'outline-up-heading
  )

(tmtxt/in '(darwin)
  (tmtxt/define-keys
      org-mode-map
    "<C-s-268632074>"   'org-metaleft
    "<C-s-268632076>"   'org-metaright
    "<C-s-268632073>"   'org-metaup
    "<C-s-268632075>"   'org-metadown
    ))

(tmtxt/define-keys
    c++-mode-map
  "C-S-<return>"    'ac-complete-clang)

(tmtxt/define-keys
    dired-mode-map
  "s-u"     'tmtxt/dired-do-shell-unmount-device
  "C-c C-r"   'tda/rsync
  "C-c C-a"   'tda/rsync-multiple-mark-file
  "C-c C-e"   'tda/rsync-multiple-empty-list
  "C-c C-d"   'tda/rsync-multiple-remove-item
  "C-c C-v"   'tda/rsync-multiple
  "C-c C-z"   'tda/zip
  "C-c C-u"   'tda/unzip
  "C-c C-t"   'tda/rsync-delete
  "C-c C-k"   'tat/kill-all
  "C-c C-n"   'tat/move-to-bottom-all
  "C-c C-s"   'tda/get-files-size
  "C-c C-q"   'tda/download-to-current-dir
  "C-c C-l"   'tda/download-clipboard-link-to-current-dir
  "C-S-n"   'dired-create-directory
  "C-S-u"   'dired-up-directory
  "C-o"     'dired-omit-mode
  "M-b" 'subword-backward
  "C-M-b" 'paredit-backward
  )

(tmtxt/define-keys
    dired-mode-map
  "s-o"     'tmtxt/dired-do-shell-open
  "s-O"     'tmtxt/dired-open-current-directory)

(tmtxt/in '(darwin)
  (tmtxt/define-keys
      dired-mode-map
    "C-c C-o"   'tmtxt/open-current-dir-in-terminal))

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

;; (tmtxt/define-keys
;;     ac-completing-map
;;   "M-/"     'ac-stop)

(tmtxt/keys 'wdired wdired-mode-map
  "TAB"   'tmtxt/mark-file-name-forward
  "S-TAB" 'tmtxt/mark-file-name-backward
  "s-o"   'tmtxt/dired-do-shell-open)

(tmtxt/keys 'projectile
    global-map
  "C-x C-d"     'projectile-dired
  "C-x i"       'projectile-invalidate-cache)

(tmtxt/keys 'helm
    global-map
  ;; "M-X"                  'helm-M-x
  ;; "C-x f"                'helm-recentf
  ;; "<C-M-S-tab>"          'tmtxt/helm
  "C-S-q" 'tmtxt/helm
  "<C-M-S-iso-lefttab>"  'tmtxt/helm
  "M-V"    'helm-show-kill-ring
  "C-S-s"     'helm-occur)

(tmtxt/keys 'helm helm-map
  "C-M-S-k"  'helm-next-source
  "C-M-S-i"  'helm-previous-source)

(tmtxt/keys 'helm-projectile
    global-map
  "C-x C-S-f"        'helm-projectile-find-file
  "C-x C-S-d"        'helm-projectile-find-dir
  "C-x C-S-s"        'helm-projectile-switch-project
  "C-x C-S-r"        'helm-projectile-ag)

(tmtxt/keys 'helm-dired-recent-dirs
    global-map
  "C-c C-y"  'helm-dired-recent-dirs-view)

(tmtxt/define-keys js2-mode-map
  "C-M-\"" 'js2-mark-defun
  "C-M-:" 'js2-mode-toggle-hide-functions
  "C-M->" 'js2-mode-toggle-element
  "C-t"  'tmtxt/delete-tern-process
  "C-M-|" 'prettier
  "s-p"   'tmtxt/toggle-prettier-line-width
  "C-M-S-d" 'tmtxt/js-doc-insert-function-doc-snippet)

(tmtxt/define-keys js2-jsx-mode-map
  "s-j"  'tmtxt/switch-to-web-mode)

(tmtxt/keys 'flycheck
    flycheck-mode-map
  "C-c C-p"     'helm-flycheck)

(tmtxt/keys 'evil
    evil-motion-state-map
  "i"        'evil-insert-state)

(tmtxt/keys 'evil
    evil-emacs-state-map
  "M-q"        'tmtxt/evil-exit-insert-state)

(tmtxt/keys 'smart-forward
    global-map
  "C-M-S-l"        'smart-forward
  "C-M-S-j"        'smart-backward
  "C-M-S-i"        'smart-up
  "C-M-S-k"        'smart-down)

(tmtxt/keys 'moz
    moz-minor-mode-map
  "C-c C-q"        'moz-send-region)

(tmtxt/define-keys comint-mode-map
  "C-M-i"    'comint-previous-matching-input-from-input
  "C-M-k"    'comint-next-matching-input-from-input)

(tmtxt/define-keys sql-mode-map
  "C-M-\\"  'sql-indent-buffer)

(tmtxt/define-keys global-map
  "C-c m"   'git-messenger:popup-message)

(tmtxt/define-keys global-map
  "C-c SPC" 'ace-jump-mode)

(tmtxt/define-keys web-mode-map
  "s-y"   'tmtxt/web-mode-change-indentation
  "s-g"   'web-mode-element-content-select
  "s-I"   'web-mode-element-parent
  "s-K"   'web-mode-element-child
  "s-L"   'web-mode-element-next
  "s-J"   'web-mode-element-previous
  "s-h"   'web-mode-element-kill
  "s-n"   'web-mode-element-select
  "C-M-|" 'prettier
  ;; web mode tag match function not defined as interactive
  "s-m"   (lambda () (interactive) (web-mode-tag-match))
  "s-p"   'tmtxt/switch-php-mode
  "s-j"   'tmtxt/switch-to-js2-jsx-mode)

(tmtxt/define-keys cider-repl-mode-map
  "C-M-i"    'cider-repl-previous-input
  "C-M-k"    'cider-repl-next-input
  "M-s-n"    'cider-repl-set-ns)

(tmtxt/define-keys cypher-mode-map
  "M-s-s"   'n4js-send-dwim)

(tmtxt/define-keys magit-status-mode-map
  "C-M-S-i"   'fmg/increase-major-tag
  "C-M-S-o"   'fmg/increase-minor-tag
  "C-M-S-p"   'fmg/increase-patch-tag)

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

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(provide 'tmtxt-key-bindings)
