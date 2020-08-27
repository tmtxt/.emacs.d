;;; haml-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "haml-mode" "haml-mode.el" (0 0 0 0))
;;; Generated autoloads from haml-mode.el

(autoload 'haml-mode "haml-mode" "\
Major mode for editing Haml files.

\\{haml-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haml-mode" '("haml-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; haml-mode-autoloads.el ends here
