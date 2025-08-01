;;; shell --- config for using shell in emacs

;;; Commentary:
;;; Author: truongtx

(require 's)
(require 'eshell)
(require 'em-smart)
(require 'exec-path-from-shell)
(require 'powershell)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ENV and PATH

;;; copy PATH from my default shell
;;; Mac
(tmtxt/in '(darwin)
  (add-to-list 'exec-path-from-shell-variables "NODE_PATH")
  (add-to-list 'exec-path-from-shell-variables "GOPATH")
  (exec-path-from-shell-initialize))
;;; Windows
(tmtxt/in '(windows-nt)
  (add-to-list 'exec-path "C:/Users/me/scoop/apps/msys2/current/usr/bin")
  (setenv "PATH" (mapconcat #'identity exec-path path-separator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eshell

;;; General config
(setq-default
 eshell-save-history-on-exit t          ;save history
 eshell-cmpl-cycle-completions t        ;TAB for suggestion
 eshell-where-to-jump 'begin
 eshell-review-quick-commands nil
 eshell-smart-space-goes-to-end t
 eshell-last-dir-ring-size 500)

;;; Buffer-local config
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local show-trailing-whitespace nil)
            (setq-local pcomplete-ignore-case t)
            (toggle-truncate-lines t)))

;;; Change buffer name to the current directory
(defun tmtxt/eshell-change-buffer-name ()
  "Change the current eshell buffer name to current directory related name"
  (let* ((dir-name (-> default-directory
                       (directory-file-name)
                       (file-name-nondirectory)))
         (new-buffer-name (s-concat "*eshell " dir-name "*")))
    (rename-buffer new-buffer-name t)))

(add-hook 'eshell-mode-hook 'tmtxt/eshell-change-buffer-name)
(add-hook 'eshell-directory-change-hook 'tmtxt/eshell-change-buffer-name)

;;; Custom prompt
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
                        "┌∩┐(ಠ_ಠ)┌∩┐ Bực VL"
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

;;; Custom function for switching Eshell buffers using Helm
(defun tmtxt/helm-eshell ()
  "Switch between eshell buffers using helm"
  (interactive)
  (helm
   :sources (list (helm-build-sync-source "Eshell buffers"
                    :candidates
                    (lambda ()
                      (cl-loop for buf in (buffer-list)
                               when (eq (buffer-local-value 'major-mode buf) 'eshell-mode)
                               collect (cons (buffer-local-value 'default-directory buf)
                                             buf)))
                    :action (list (cons "Switch to buffer" #'switch-to-buffer))
                    )))
  :buffer "*helm eshell*"
  :prompt "Switch to Eshell buffer: ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Powershell config
(add-hook 'powershell-mode-hook
          (lambda ()
            (auto-complete-mode)))

(provide 'tmtxt-shell)
;;; tmtxt-shell.el ends here
