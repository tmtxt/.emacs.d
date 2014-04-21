(tmtxt/set-up 'twittering-mode
  (setq twittering-status-format
		"%i %s, %FACE[ublt-twitter-meta-face]{%@ from %f%L%r%R}\n\t%t"
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

(provide 'tmtxt-twitter)
