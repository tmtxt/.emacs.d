;;; Commentary:

;; This file provides support for running Clojure tests (using the
;; clojure.test framework) via SLIME and seeing feedback in the test
;; buffer about which tests failed or errored.

;;; Installation:

;; If you use ELPA, you can install via the M-x package-list-packages
;; interface. This is preferrable as you will have access to updates
;; automatically.

;; If you need to install by hand for some reason:

;; (0) Add this file to your load-path, usually the ~/.emacs.d directory.
;; (1) Either:
;;     Add these lines to your .emacs:
;;      (autoload 'clojure-test-mode "clojure-test-mode" "Clojure test mode" t)
;;      (autoload 'clojure-test-maybe-enable "clojure-test-mode" "" t)
;;      (add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)
;;
;;     Or generate autoloads with the `update-directory-autoloads' function.

;; This library does not currently support clojure.contrib.test-is
;; from Clojure Contrib's 1.0-compatibility branch. If you need it,
;; please use version 1.2 of clojure-test-mode:

;; http://github.com/technomancy/clojure-mode/tree/test-1.2

