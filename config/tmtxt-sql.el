(setq sql-postgres-login-params
      '((user :default "postgres")
        (database :default "postgres")
        (server :default "localhost")
        (port :default 5432)))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(setq sql-connection-alist
      '((mooc.dev (sql-product 'postgres)
                  (sql-port 54321)
                  (sql-server "localhost")
                  (sql-user "mooc")
                  (sql-database "mooc_dev"))
        (icon.dev (sql-product 'postgres)
                  (sql-port 54321)
                  (sql-server "localhost")
                  (sql-user "icon")
                  (sql-database "icon"))))

(defun tmtxt/sql-icon-dev ()
  (interactive)
  (tmtxt/sql-connect 'postgres 'icon.dev))

(defun tmtxt/sql-mooc-dev ()
  (interactive)
  (tmtxt/sql-connect 'postgres 'mooc.dev))

(defun tmtxt/sql-connect (product connection)
  ;; load the password
  (require 'tmtxt-identica "tmtxt-identica.el.gpg")

  ;; update the password to the sql-connection-alist
  (let ((connection-info (assoc connection sql-connection-alist))
        (sql-password (car (last (assoc connection tmtxt-sql-password))))
        new-connection-alist)
    (delete sql-password connection-info)
    (nconc connection-info `((sql-password ,sql-password)))
    (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
    (add-to-list 'sql-connection-alist connection-info))

  ;; connect to database
  (setq sql-product product)
  (sql-connect connection))

(defvar tmtxt/sql-servers-list
  '(("Icon Dev" tmtxt/sql-icon-dev)
    ("Mooc Dev" tmtxt/sql-mooc-dev))
  "Alist of server name and the function to connect")

(defun tmtxt/sql-connect-server (func)
  "Connect to the input server using tmtxt/sql-servers-list"
  (interactive
   (helm-comp-read "Select server: " tmtxt/sql-servers-list))
  (funcall func))

(provide 'tmtxt-sql)
