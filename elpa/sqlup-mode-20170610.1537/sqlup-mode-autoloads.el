;;; sqlup-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sqlup-mode" "sqlup-mode.el" (0 0 0 0))
;;; Generated autoloads from sqlup-mode.el

(autoload 'sqlup-mode "sqlup-mode" "\
Capitalizes SQL keywords for you.

\(fn &optional ARG)" t nil)

(autoload 'sqlup-capitalize-keywords-in-region "sqlup-mode" "\
Call this function on a region to capitalize the SQL keywords therein.

\(fn START-POS END-POS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sqlup-mode" '("sqlup-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sqlup-mode-autoloads.el ends here
