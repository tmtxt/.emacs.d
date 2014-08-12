(tmtxt/set-up 'sql
  (setq sql-postgres-login-params
        '((user :default "postgres")
          (database :default "postgres")
          (server :default "localhost")
          (port :default 5432)))

  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (toggle-truncate-lines t))))

(provide 'tmtxt-sql)
