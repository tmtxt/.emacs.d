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
  '("C-M-j" "C-M-l"))

(tmtxt/define-keys
	global-map
  "C-c f"		'find-file-in-project
  "C-+"			'text-scale-increase
  "C--"			'text-scale-decrease
  "C-M-h"		'backward-kill-word
  "C-s"			'isearch-forward-regexp
  "\C-r"	   	'isearch-backward-regexp
  "M-%"			'query-replace-regexp
  "C-M-s"		'isearch-forward
  "C-M-r"		'isearch-backward
  "C-M-%"		'query-replace
  "C-x C-i"		'imenu
  "C-x M-f"		'ido-find-file-other-window
  "C-c y"		'bury-buffer
  "C-c r"		'revert-buffer
  "C-x m"		'eshell
  "C-x M"		(lambda () (interactive) (eshell t))	;Start a new eshell even if one is active.
  "C-c q"		'join-line
  "C-c g"		'magit-status
  "C-x c"		'compile
  "C-S-s"		'helm-swoop
  "C-c l"		'org-store-link
  "C-c a"		'org-agenda
  "C-c b"		'org-iswitchb

  ;; buffer managements
  "C-S-<tab>"			'tmtxt/switch-to-last-buffer ;OSX
  "<C-S-iso-lefttab>"	'tmtxt/switch-to-last-buffer ;Linux
  "C-x C-b"				'ibuffer
  "s-k"					'kill-this-buffer
  )

;;; TODO: rebind these keys
;;; M-j (comment-indent-new-line)

(tmtxt/define-keys
	key-translation-map

  ;; ergonomic layout
  "M-i"			"C-p"
  "M-k"			"C-n"
  "M-j"			"C-b"
  "M-l"			"C-f"
  "M-I"			"M-v"
  "M-K"			"C-v"
  "M-L"			"C-e"
  "M-J"			"C-a"
  "M-o"			"M-f"
  "M-u"			"M-b"
  "M-n"			"M-<"
  "M-N"			"M->"
  )

(tmtxt/define-keys
	org-mode-map

  ;; move meta up/down/left/right
  "C-s-j"				'org-metaleft
  "C-s-l"				'org-metaright
  "C-s-i"				'org-metaup
  "C-s-k"				'org-metadown

  ;; shift meta up/down/left/right
  "C-S-s-j"				'org-shiftmetaleft
  "C-S-s-l"				'org-shiftmetaright
  "C-S-s-i"				'org-shiftmetaup
  "C-S-s-k"				'org-shiftmetadown

  ;; navigation between headings
  "C-M-S-k"				'outline-next-visible-heading
  "C-M-S-i"				'outline-previous-visible-heading
  "C-M-S-l"				'org-forward-heading-same-level
  "C-M-S-j"				'org-backward-heading-same-level
  "C-M-S-u"				'outline-up-heading
  "<C-tab>"				'ido-switch-buffer
  )

(tmtxt/in '(darwin)
  (tmtxt/define-keys
	  org-mode-map
	"<C-s-268632074>"		'org-metaleft
	"<C-s-268632076>"		'org-metaright
	"<C-s-268632073>"		'org-metaup
	"<C-s-268632075>"		'org-metadown
	))

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
