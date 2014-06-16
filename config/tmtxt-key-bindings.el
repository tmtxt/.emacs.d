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
  '("C-M-j" "C-M-l" "C-x C-d"))

(tmtxt/define-keys
    global-map
  "C-c f"   'find-file-in-project
  "C-+"     'text-scale-increase
  "C--"     'text-scale-decrease
  "M-%"     'query-replace-regexp
  "C-M-%"   'query-replace
  "C-x C-i" 'imenu
  "C-x M-f" 'ido-find-file-other-window
  "C-c y"   'bury-buffer
  "C-c r"   'revert-buffer
  "C-c g"   'magit-status
  "C-x c"   'compile
  "M-;"     'comment-dwim
  "M-:"     'evilnc-comment-or-uncomment-lines

  "M-x"     'smex
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
  "s-k"                'kill-this-buffer
  "<C-tab>"            'ido-switch-buffer

  ;; ECB
  "C-x C-;"   'ecb-activate
  "C-x C-'"   'tmtxt/ecb-deactivate
  "C-;"       'tmtxt/ecb-show-ecb-windows
  "C-'"       'tmtxt/ecb-hide-ecb-windows

  "C-S-f"     'yas-expand
  "M-="       'er/expand-region         ;expand region
  
  )

;;; TODO: rebind these keys
;;; M-j (comment-indent-new-line)

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
  "M-W"     "s-k"
  "M-1"     "C-c C-a"
  "M-2"     "C-x o"
  )

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
  "<C-tab>"       'ido-switch-buffer
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
  "s-b"     'tmtxt/dired-mark-backward
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
  )

(tmtxt/define-keys
    dired-mode-map
  "s-o"     'tmtxt/dired-do-shell-open
  "s-O"     'tmtxt/dired-open-current-directory)

(tmtxt/in '(darwin)
  (tmtxt/define-keys
      dired-mode-map
    "C-c C-o"   'tmtxt/open-current-dir-in-terminal))

(tmtxt/keys 'ecb
    ecb-mode-map
  "C-M-<"     'ecb-toggle-compile-window-height
  "C-)"       'ecb-goto-window-edit1
  "C-!"       'ecb-goto-window-directories
  "C-@"       'ecb-goto-window-sources
  "C-#"       'ecb-goto-window-methods
  "C-%"       'ecb-goto-window-compilation
  )

(tmtxt/define-keys
    hs-minor-mode-map
  "C-c C-h"   'hs-hide-block
  "C-c C-d"   'hs-show-block
  "C-c C-t"   'hs-toggle-hiding
  "C-c C-;"   'hs-hide-all
  "C-c C-'"   'hs-show-all
  "C-c C-l"   'hs-hide-level
  )

(tmtxt/define-keys
    ac-completing-map
  "M-/"     'ac-stop)

(tmtxt/keys 'wdired wdired-mode-map
  "TAB"   'tmtxt/mark-file-name-forward
  "S-TAB" 'tmtxt/mark-file-name-backward
  "s-o"   'tmtxt/dired-do-shell-open)

(tmtxt/set-up 'projectile
  (tmtxt/keys 'projectile
      global-map
    ;; projectile
    "C-x C-S-f"   'projectile-find-file
    "C-x C-d"     'projectile-dired
    "C-x C-S-d"   'projectile-find-dir))

(tmtxt/keys 'helm
    global-map
  "M-X"                  'helm-M-x
  "C-x f"                'helm-recentf
  "<C-M-S-tab>"          'tmtxt/helm
  "<C-M-S-iso-lefttab>"  'tmtxt/helm)

(tmtxt/set-up 'helm-projectile
  (tmtxt/keys 'helm-projectile
      global-map
    "C-x C-S-M-f"        'helm-projectile))

(tmtxt/set-up 'helm-swoop
  (tmtxt/keys 'helm-swoop
      global-map
    "C-S-s"        'helm-swoop))

(tmtxt/keys 'helm-dired-recent-dirs
    global-map
  "C-c C-y"  'helm-dired-recent-dirs-view)

(tmtxt/define-keys js2-minor-mode-map
  "C-M-?" 'ac-js2-jump-to-definition
  "C-M-\"" 'js2-mark-defun
  "C-M-:" 'js2-mode-toggle-hide-functions
  "C-M->" 'js2-mode-toggle-element
  "C-t"  'tmtxt/delete-tern-process)

(tmtxt/set-up 'helm-flycheck
  (tmtxt/keys 'flycheck
      flycheck-mode-map
    "C-c C-p"     'helm-flycheck))

(tmtxt/set-up 'ac-helm
  (tmtxt/keys 'ac-helm
      global-map
    "C-:"        'ac-complete-with-helm)

  (tmtxt/keys 'ac-helm
      ac-complete-mode-map
    "C-:"        'ac-complete-with-helm))

(tmtxt/keys 'evil
    evil-motion-state-map
  "i"        'evil-insert-state)

(tmtxt/set-up 'smart-forward
  (tmtxt/keys 'smart-forward
      global-map
    "C-M-S-l"        'smart-forward
    "C-M-S-j"        'smart-backward
    "C-M-S-i"        'smart-up
    "C-M-S-k"        'smart-down))

(tmtxt/keys 'moz
    moz-minor-mode-map
  "C-c C-q"        'moz-send-region)

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
