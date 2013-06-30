;;; config for those google package

;;; google-this
;;; google search from emacs
;;; C-x g RET - google search
;;; C-x g t - google search marked region
;;; C-x g w - google search word at point
;;; C-x g s - google search symbol at point
;;; C-x g l - google search current line
;;; C-x g r - google search region
(tmtxt/set-up 'google-this
  (google-this-mode 1))

;;; google translate
(tmtxt/set-up 'google-translate
  (global-set-key (kbd "C-x g a") 'google-translate-at-point)
  (global-set-key (kbd "C-x g q") 'google-translate-query-translate))
;;; default endlish -> vietnamese
(setq google-translate-default-source-language "en"
	  google-translate-default-target-language "vi")

;;; finally provide the lib
(provide 'tmtxt-google)
