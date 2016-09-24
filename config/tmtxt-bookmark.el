;;; this is my config for bookmark and bookmark+

;;; require
(require 'bookmark+)

(setq
 bookmark-default-file "~/emacs.bookmarks"
 bmkp-last-as-first-bookmark-file "~/emacs.bookmarks")

;;; put last-selected bookmark on top
;;; http://www.emacswiki.org/emacs/BookMarks#toc6
(defadvice bookmark-jump (after bookmark-jump activate)
  (let ((latest (bookmark-get-bookmark bookmark)))
    (setq bookmark-alist (delq latest bookmark-alist))
    (add-to-list 'bookmark-alist latest)))

;;; finally provide the library to call
(provide 'tmtxt-bookmark)
