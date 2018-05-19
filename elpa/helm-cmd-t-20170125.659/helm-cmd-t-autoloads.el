;;; helm-cmd-t-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "helm-cmd-t" "helm-cmd-t.el" (23295 61929 545536
;;;;;;  144000))
;;; Generated autoloads from helm-cmd-t.el

(autoload 'helm-cmd-t "helm-cmd-t" "\
Choose file from current repo.

With prefix arg C-u, run `helm-cmd-t-repos'.

\(fn &optional ARG)" t nil)

(autoload 'helm-cmd-t-repos "helm-cmd-t" "\
Manage helm-cmd-t caches.

\(fn &optional PRESELECT-ROOT)" t nil)

(autoload 'helm-cmd-t-grep "helm-cmd-t" "\
Grep action run from `helm-cmd-t-repos'.

Redirect to the correct grep based on `candidate-buffer'.

\(fn CANDIDATE-BUFFER &optional GLOBS)" t nil)

;;;***

;;;### (autoloads nil nil ("helm-C-x-b.el" "helm-cmd-t-find.el" "helm-cmd-t-pkg.el")
;;;;;;  (23295 61929 547165 72000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; helm-cmd-t-autoloads.el ends here
