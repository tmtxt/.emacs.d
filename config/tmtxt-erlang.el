;;; config for erlang
(require 'erlang-start)

(dolist (f '(auto-complete-mode
             flycheck-mode
             tmtxt-paredit-nonlisp
             tmtxt/prog-mode-setup
             ))
  (add-hook 'erlang-mode-hook f))

(provide 'tmtxt-erlang)
