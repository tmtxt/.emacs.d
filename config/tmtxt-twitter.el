(tmtxt/set-up 'twittering-mode
  (defface tmtxt-twitter-meta-face
    '((t (:inherit font-lock-comment-face)))
    "Twitter face for important text")
  (setq
   twittering-status-format
   "%i %s, %FACE[tmtxt-twitter-meta-face]{%@ from %f%L%r%R}\n\t%t"
   twittering-url-show-status nil
   twittering-timer-interval 600
   twittering-use-master-password t
   twittering-use-icon-storage t
   twittering-display-remaining t
   twittering-number-of-tweets-on-retrieval 100
   twittering-icon-mode t)
  (add-hook 'twittering-mode-hook (tmtxt/on-fn 'hl-line-mode))
  (define-key twittering-edit-mode-map (kbd "C-c C-t")
    'twittering-tinyurl-replace-at-point))

;;; notification when new tweet
(tmtxt/in '(darwin)
  (defun tmtxt/notify-new-tweets ()
    "Notify when new tweet come"
    (if (executable-find "terminal-notifier")
        (shell-command "terminal-notifier -message 'You have new tweets' -title 'Twittering mode' -sender 'org.gnu.Emacs'")
      (message "No terminal-notifier installed")))
  (add-hook 'twittering-new-tweets-hook 'tmtxt/notify-new-tweets))

(provide 'tmtxt-twitter)
