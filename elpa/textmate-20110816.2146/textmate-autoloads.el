;;; textmate-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "textmate" "textmate.el" (0 0 0 0))
;;; Generated autoloads from textmate.el

(defvar textmate-mode nil "\
Non-nil if Textmate mode is enabled.
See the `textmate-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `textmate-mode'.")

(custom-autoload 'textmate-mode "textmate" nil)

(autoload 'textmate-mode "textmate" "\
TextMate Emulation Minor Mode

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "textmate" '("textmate-" "allow-line-as-region-for-function" "*textmate-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; textmate-autoloads.el ends here
