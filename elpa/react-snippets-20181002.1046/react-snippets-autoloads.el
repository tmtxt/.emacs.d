;;; react-snippets-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "react-snippets" "react-snippets.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from react-snippets.el

(autoload 'react-snippets-initialize "react-snippets" "\


\(fn)" nil nil)

(eval-after-load 'yasnippet '(react-snippets-initialize))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "react-snippets" '("react-snippets-root")))

;;;***

;;;### (autoloads nil nil ("react-snippets-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; react-snippets-autoloads.el ends here
