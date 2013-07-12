;;; navigation util

;;; include common lisp
(eval-when-compile (require 'cl))

;;; Better C-x C-x
;;; exchange point and mark but not mark the region
;; `http://www.masteringemacs.org/articles/2010/12/22/fixing-mark-commands-transient-mark-mode/'
(defun tmtxt/exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark]
  'tmtxt/exchange-point-and-mark-no-activate)

;;; Better C-a
(defun tmtxt/back-to-indentation-or-line-beginning ()
  "Go back to indentation, or if already there, to the beginning
of line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))
(define-key global-map [remap move-beginning-of-line]
  'tmtxt/back-to-indentation-or-line-beginning)

;;; Swap windows
;;; `http://sites.google.com/site/steveyegge2/my-dot-emacs-file'
(defun tmtxt/swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))

;;; smooth scroll
;;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling    
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;; custom key bindings
;; switch to other window in the same frame
(global-set-key (kbd "C-S-o") 'other-window)

;;; finally, provide the library
(provide 'tmtxt-navigation)
