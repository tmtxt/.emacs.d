;;; config for working with clojure

;;; include config for working with lisp since clojure is a lisp like language
(require 'tmtxt-lisp)

(add-hook 'clojure-mode-hook 'tmtxt-pretty-fn)
(add-hook 'clojurescript-mode-hook 'tmtxt-pretty-fn)

(provide 'tmtxt-clojure)
