(tmtxt/set-up 'sql
  (setq sql-postgres-login-params
        '((user :default "postgres")
          (database :default "postgres")
          (server :default "localhost")
          (port :default 5432)))

  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (toggle-truncate-lines t)))

  (setq sql-connection-alist
        '((icon.dev (sql-product 'postgres)
                    (sql-port 54321)
                    (sql-server "localhost")
                    (sql-user "icon")
                    (sql-password "vagrant")
                    (sql-database "icon"))
          (mooc.dev (sql-product 'postgres)
                    (sql-port 54321)
                    (sql-server "localhost")
                    (sql-user "mooc")
                    (sql-password "vagrant")
                    (sql-database "mooc_dev"))))

  (defun tmtxt/sql-icon-dev ()
    (interactive)
    (setq sql-product 'postgres)
    (sql-connect 'icon.dev))

  (defun tmtxt/sql-mooc-dev ()
    (interactive)
    (setq sql-product 'postgres)
    (sql-connect 'mooc.dev)))

(provide 'tmtxt-sql)
