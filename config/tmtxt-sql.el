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
                  (sql-database "icon"))
        (pedigree.dev (sql-product 'postgres)
                      (sql-port 54321)
                      (sql-server "localhost")
                      (sql-user "pedigree")
                      (sql-database "pedigree"))
        (sugar.sea.dev (sql-product 'postgres)
                       (sql-port 54321)
                       (sql-server "localhost")
                       (sql-user "sugar_and_sea")
                       (sql-database "sugar_and_sea"))))

(defun tmtxt/sql-connect (product connection)
  ;; load the password
  (require 'tmtxt-identica "tmtxt-identica.el.gpg")

  ;; update the password to the sql-connection-alist
  (let ((connection-info (assoc connection sql-connection-alist))
        (sql-password (car (last (assoc connection tmtxt-sql-password)))))
    (delete sql-password connection-info)
    (nconc connection-info `((sql-password ,sql-password)))
    (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
    (add-to-list 'sql-connection-alist connection-info))

  ;; connect to database
  (setq sql-product product)
  (if current-prefix-arg
      (sql-connect connection connection)
    (sql-connect connection)))

(defvar tmtxt/sql-servers-list
  '(("Icon Dev" icon.dev)
    ("Mooc Dev" mooc.dev)
    ("Pedigree Dev" pedigree.dev)
    ("Sugar and Sea Dev" sugar.sea.dev))
  "Alist of server name and the connection name")

(defun tmtxt/sql-connect-server (connection)
  "Connect to the input server using tmtxt/sql-servers-list"
  (interactive
   (helm-comp-read "Select server: " tmtxt/sql-servers-list))

  ;; get the sql connection info and product from the sql-connection-alist
  (let* ((connection-info (assoc connection sql-connection-alist))
         (connection-product (nth 1 (nth 1 (assoc 'sql-product connection-info)))))
    ;; connect to server
    (tmtxt/sql-connect connection-product connection)))

;;; sql up mode
;; (tmtxt/set-up 'sqlup-mode
;;   (add-hook 'sql-mode-hook 'sqlup-mode)
;;   (add-hook 'sql-interactive-mode-hook 'sqlup-mode))

(provide 'tmtxt-sql)
