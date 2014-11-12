;;; thesaurus-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (thesaurus-set-bhl-api-key-from-file thesaurus-choose-synonym-and-replace
;;;;;;  thesaurus-get-synonyms) "thesaurus" "thesaurus.el" (21597
;;;;;;  34351 0 0))
;;; Generated autoloads from thesaurus.el

(autoload 'thesaurus-get-synonyms "thesaurus" "\
retrieve synonyms for the given word, either from the cache,
or, if there is no cache hit, then from the remote service.

\(fn WORD)" nil nil)

(autoload 'thesaurus-choose-synonym-and-replace "thesaurus" "\
The main interactive entry point into the `thesaurus.el' capability.
Invoke this interactively, and the fn will prompt the user to
confirm the word to be looked up.  It will then retrieve a list
of synonyms for the word, either from the cache or from a remote
service, and prompt the user with a list of possible
replacements.  If the user chooses a replacement, the original
word in the buffer will be removed and the replacement will be
inserted in its place.

\(fn WORD)" t nil)

(autoload 'thesaurus-set-bhl-api-key-from-file "thesaurus" "\
A way to set the API key for BigHugeLabs with the contents of
a text file. That text file should contain the key obtained from
BHL during registration.

\(fn FILENAME)" t nil)

;;;***

;;;### (autoloads nil nil ("thesaurus-pkg.el") (21597 34351 251128
;;;;;;  0))

;;;***

(provide 'thesaurus-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; thesaurus-autoloads.el ends here
