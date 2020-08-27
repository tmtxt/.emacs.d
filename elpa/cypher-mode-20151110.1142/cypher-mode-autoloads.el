;;; cypher-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cypher-mode" "cypher-mode.el" (0 0 0 0))
;;; Generated autoloads from cypher-mode.el

(autoload 'cypher-mode "cypher-mode" "\
Major mode for editing web templates.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.cypher\\'" . cypher-mode))

(add-to-list 'auto-mode-alist '("\\.cyp\\'" . cypher-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cypher-mode" '("cypher-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cypher-mode-autoloads.el ends here
