;;; vlf-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (vlf) "vlf" "vlf.el" (21597 34401 0 0))
;;; Generated autoloads from vlf.el

(autoload 'vlf "vlf" "\
View Large FILE in batches.  When MINIMAL load just a few bytes.
You can customize number of bytes displayed by customizing
`vlf-batch-size'.
Return newly created buffer.

\(fn FILE &optional MINIMAL)" t nil)

;;;***

;;;### (autoloads (vlf-ediff-files) "vlf-ediff" "vlf-ediff.el" (21597
;;;;;;  34402 0 0))
;;; Generated autoloads from vlf-ediff.el

(autoload 'vlf-ediff-files "vlf-ediff" "\
Run batch by batch ediff over FILE-A and FILE-B.
Files are processed with VLF with BATCH-SIZE chunks.
Requesting next or previous difference at the end or beginning
respectively of difference list, runs ediff over the adjacent chunks.

\(fn FILE-A FILE-B BATCH-SIZE)" t nil)

;;;***

;;;### (autoloads (vlf-occur-load) "vlf-occur" "vlf-occur.el" (21597
;;;;;;  34402 0 0))
;;; Generated autoloads from vlf-occur.el

(autoload 'vlf-occur-load "vlf-occur" "\
Load serialized `vlf-occur' results from current buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("vlf-base.el" "vlf-follow.el" "vlf-integrate.el"
;;;;;;  "vlf-pkg.el" "vlf-search.el" "vlf-tune.el" "vlf-write.el")
;;;;;;  (21597 34402 282959 0))

;;;***

(provide 'vlf-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vlf-autoloads.el ends here
