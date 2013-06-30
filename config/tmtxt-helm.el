;;; config for helm

;;; some required helm packages
(require 'helm-config)
(require 'helm-match-plugin)
(require 'helm-regexp)
(require 'helm-buffers)
(require 'helm-files)

;;; some config
(setq
 ;; highlight matches after this many seconds
 helm-mp-highlight-delay 0.7

 ;; Minimum length of pattern to highlight.
 helm-mp-highlight-threshold 4

 ;; Better-looking separator for multi-line sources
 helm-candidate-separator "────────────────────────────────────────────────────────────────────────────────"
 ;; auto get the word at point and put it into helm minibuffer
 ;; helm-yank-symbol-first t
 
 helm-maybe-use-default-as-input t)

;;; finally provide the library
(provide 'tmtxt-helm)
