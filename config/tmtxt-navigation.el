;;; require
(require 'cl)

;;; Better C-x C-x
;;; exchange point and mark but not mark the region
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

(windmove-default-keybindings) ;; Shift+direction

;;; finally, provide the library
(provide 'tmtxt-navigation)
