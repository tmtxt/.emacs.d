;;; shell --- config for using shell in emacs

;;; Commentary:
;;; Author: truongtx

(require 's)
(require 'eshell)
(require 'em-smart)
(require 'exec-path-from-shell)

;;; Code:

;;; copy PATH from my default shell
(tmtxt/in '(darwin)
  (add-to-list 'exec-path-from-shell-variables "NODE_PATH")
  (add-to-list 'exec-path-from-shell-variables "GOPATH")
  (exec-path-from-shell-initialize))

;;; disable trailing whitespace for term modes
(dolist (hook '(eshell-mode-hook
                term-mode-hook))
  (add-hook hook
            (lambda ()
              (setq-local show-trailing-whitespace nil))))

;;; the rest is eshell config
;;; eshell
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(setq eshell-last-dir-ring-size 500)
(setq-default
 eshell-save-history-on-exit t          ;save history
 eshell-cmpl-cycle-completions t        ;TAB for suggestion
 eshell-buffer-shorthand t              ;shorthand buffer name
 )

;;; custom functions to activate eshell
(defun tmtxt/eshell (&optional arg)
  "Wrapper around default eshell command to keep the exec path and other env as expected"
  (interactive "P")
  (let ((buf (call-interactively 'eshell arg))
        (cd-eshell (lambda ()
                     (eshell/cd default-directory)
                     (eshell-reset))))
    (unless (get-buffer-process buf)
      (funcall cd-eshell))))

(defun tmtxt/eshell-change-buffer-name ()
  "Change the current eshell buffer name to current directory related name"
  (let* ((dir-name (-> default-directory
                       (directory-file-name)
                       (file-name-nondirectory)))
         (new-buffer-name (s-concat "*eshell " dir-name "*")))
    (rename-buffer new-buffer-name t)))

;;; hook
(add-hook 'eshell-mode-hook
          (lambda ()
            (toggle-truncate-lines t)
            (setq-local pcomplete-ignore-case t)))
(add-hook 'eshell-mode-hook 'tmtxt/eshell-change-buffer-name)

(add-hook 'eshell-directory-change-hook 'tmtxt/eshell-change-buffer-name)

;;; autojump
(eval-after-load 'eshell
  '(require 'eshell-autojump nil t))

;;; eshell prompt
(defun tmtxt/shortened-path (path max-len)
  "Return a modified version of `path', replacing some components
      with single characters starting from the left to try and get
      the path down to `max-len'"
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str (if (= 0 (length (car components)))
                                "/"
                              (string (elt (car components) 0) ?/)))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (reduce (lambda (a b) (concat a "/" b)) components))))

(defun tmtxt/random-kamoji ()
  (let* ((kamoji-list '("(╯‵□′)╯︵┻━┻ Lộn cái bàn"
                        "［(－－)］ZZzzz"
                        "(o_O) Lại bug"
                        "( ⊙△⊙) - noooooo!!!!"
                        "(￣へ ￣ 凸 F**k"
                        "┐(´～`)┌ [No idea]"
                        ))
         (idx (random (length kamoji-list)))
         (kamoji (nth idx kamoji-list)))
    kamoji))

(defun tmtxt/eshell-prompt-function ()
  (let* ((user-name (user-login-name))
         (pwd (tmtxt/shortened-path (eshell/pwd) 40))
         (kamoji (tmtxt/random-kamoji))
         (len (+ (length user-name)
                 (length pwd)
                 (length kamoji)
                 10))
         (line-len (- (window-width) len))
         (line (s-repeat line-len " ")))
    (concat
     (propertize user-name 'face `(:foreground "#00DD00"))
     " "
     (propertize pwd 'face `(:foreground "#00CDCD" :weight bold))
     " "
     (propertize kamoji 'face `(:foreground "#B8860B"))
     " "
     (propertize line 'face `(:strike-through t))
     "\n➤ "
     )))
(setq eshell-prompt-function 'tmtxt/eshell-prompt-function)


(provide 'tmtxt-shell)
;;; tmtxt-shell.el ends here
