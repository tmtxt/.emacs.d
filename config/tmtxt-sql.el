(tmtxt/set-up 'sql
  (setq sql-postgres-login-params
        '((user :default "postgres")
          (database :default "postgres")
          (server :default "localhost"))))

(provide 'tmtxt-sql)
