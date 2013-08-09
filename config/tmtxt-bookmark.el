;;; this is my config for bookmark and bookmark+

;;; require package
(require 'bookmark+)

;;; put last-selected bookmark on top
;;; http://www.emacswiki.org/emacs/BookMarks#toc6
(defadvice bookmark-jump (after bookmark-jump activate)
  (let ((latest (bookmark-get-bookmark bookmark)))
    (setq bookmark-alist (delq latest bookmark-alist))
    (add-to-list 'bookmark-alist latest)))

;;; some key bindings for bookmarking
(global-set-key (kbd "C-c m") 'bookmark-set)
(global-set-key (kbd "C-c b") 'bookmark-jump)
(global-set-key (kbd "C-c l") 'bookmark-bmenu-list)

;;; finally provide the library to call
(provide 'tmtxt-bookmark)
