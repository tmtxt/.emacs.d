(require 'window-numbering)
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

(tmtxt/undefine-keys
    global-map
  '("C-M-j" "C-M-l" "C-x C-d" "s-m" "<f2> <f2>"))

(tmtxt/undefine-keys
    paredit-mode-map
  '("C-j"))

(tmtxt/define-keys
    global-map
  "C-c f"   'find-file-in-project
  "C-+"     'text-scale-increase
  "C--"     'text-scale-decrease
  "M-%"     'query-replace-regexp
  "C-M-%"   'query-replace
  "C-x C-i" 'helm-imenu
  "C-c y"   'bury-buffer
  "C-c r"   'revert-buffer
  "C-c g"   'magit-status
  "C-x c"   'compile
  "M-;"     'comment-dwim
  "M-:"     'evilnc-comment-or-uncomment-lines
  "s-&"     'kill-this-buffer

  "M-x"     'helm-M-x
  "C-M-\""  'mark-defun

  ;; editing
  "C-c q"   'join-line
  "C-c C-a" 'tmtxt/select-all-line
  "C-S-a"   'tmtxt/select-all-line
  "C-M-\\"  'tmtxt/indent-region-or-buffer
  "C-x F"   'tmtxt/find-file-as-root
  "C-S-j"   'textmate-shift-left
  "C-S-l"   'textmate-shift-right
  "C-m"     'newline-and-indent
  "C-M-u"   'upcase-word
  "C-M-j"   'downcase-word
  "M-/"     'hippie-expand
  "s-y"     'tmtxt/change-indentation-locally
  "M-G"     'kill-whole-line

  ;; isearch
  "C-s"     'isearch-forward-regexp
  "\C-r"    'isearch-backward-regexp
  "C-M-s"   'isearch-forward
  "C-M-r"   'isearch-backward

  ;; eshell
  "C-x m"   'eshell
  "C-x M"   (lambda () (interactive) (eshell t)) ;Start a new eshell even if one is active.

  ;; org mode
  "C-c l"   'org-store-link
  "C-c a"   'org-agenda
  "C-c b"   'org-iswitchb

  ;; buffer managements
  "C-S-<tab>"          'tmtxt/switch-to-last-buffer ;OSX
  "<C-S-iso-lefttab>"  'tmtxt/switch-to-last-buffer ;Linux
  "C-x C-b"            'ibuffer
  "<C-tab>"            'switch-to-buffer

  "M-="       'er/expand-region         ;expand region
  "C-M-S-s"   'tmtxt/sql-connect-server
  "C-M-S-c"   'tmtxt/switch-to-cider-repl

  "s-1"    'select-window-1
  "s-2"    'select-window-2
  "s-3"    'select-window-3

  "{"     'paredit-open-curly
  "}"     'paredit-close-curly
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
  "M-1"     "C-c C-a"
  "M-2"     "C-x o"
  "M-m"     "C-m"
  "M-SPC"   "C-SPC"
  "M-g"     "C-k"
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

(tmtxt/define-keys
    ac-completing-map
  "M-/"     'ac-stop)

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
  "M-X"                  'helm-M-x
  "C-x f"                'helm-recentf
  "<C-M-S-tab>"          'tmtxt/helm
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
  "C-x C-S-s"        'helm-projectile-switch-project)

(tmtxt/keys 'helm-dired-recent-dirs
    global-map
  "C-c C-y"  'helm-dired-recent-dirs-view)

(tmtxt/define-keys js2-mode-map
  "C-M-\"" 'js2-mark-defun
  "C-M-:" 'js2-mode-toggle-hide-functions
  "C-M->" 'js2-mode-toggle-element
  "C-t"  'tmtxt/delete-tern-process
  "C-M-\\" 'web-beautify-js
  "C-M-S-d" 'tmtxt/js-doc-insert-function-doc-snippet)

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
  "s-i"   'web-mode-element-parent
  "s-k"   'web-mode-element-child
  ;; "s-l"   'web-mode-element-next
  "s-j"   'web-mode-element-previous
  "s-h"   'web-mode-element-kill
  "s-n"   'web-mode-element-select
  ;; web mode tag match function not defined as interactive
  "s-m"   (lambda () (interactive) (web-mode-tag-match))
  "s-p"   'tmtxt/switch-php-mode)

(tmtxt/define-keys cider-repl-mode-map
  "C-M-i"    'cider-repl-previous-input
  "C-M-k"    'cider-repl-next-input
  "M-s-n"    'cider-repl-set-ns)

(tmtxt/define-keys cypher-mode-map
  "M-s-s"   'n4js-send-dwim)

(add-hook
 'eshell-mode-hook
 (lambda ()
   (tmtxt/define-keys eshell-mode-map
     "C-M-i" 'eshell-previous-input
     "C-M-k" 'eshell-next-input
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
