(setq sql-postgres-login-params
      '((user :default "postgres")
        (database :default "postgres")
        (server :default "localhost")
        (port :default 5432)))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)
            (setq-local show-trailing-whitespace nil)
            (auto-complete-mode t)))

;;; server list
(setq sql-connection-alist
      '((icon.dev (sql-product 'postgres)
                  (sql-port 54321)
                  (sql-server "localhost")
                  (sql-user "icon")
                  (sql-database "icon"))
        (pedigree.dev (sql-product 'postgres)
                      (sql-port 9255)
                      (sql-server "localhost")
                      (sql-user "vagrant")
                      (sql-database "pedigree"))
        (carrier.adapter (sql-product 'mysql)
                         (sql-port 3306)
                         (sql-server "carrier_adapter.host")
                         (sql-user "root")
                         (sql-database "carrier_adapter"))
        (oms.local (sql-product 'mysql)
                   (sql-port 3306)
                   (sql-server "oms.local")
                   (sql-user "oms_local")
                   (sql-database "oms_local"))
        (paul.ann (sql-product 'mysql)
                  (sql-port 3306)
                  (sql-server "10.103.1.17")
                  (sql-user "annh")
                  (sql-database "annh_alice_my"))))

;;; TODO update this function
(defun tmtxt/sql-connect-server (connection)
  "Connect to the input server using tmtxt/sql-servers-list"
  (interactive
   (helm-comp-read "Select server: " (mapcar (lambda (item)
                                               (list
                                                (symbol-name (nth 0 item))
                                                (nth 0 item)))
                                             sql-connection-alist)))
  ;; password
  (require 'tmtxt-identica "tmtxt-identica.el.gpg")
  ;; get the sql connection info and product from the sql-connection-alist
  (let* ((connection-info (assoc connection sql-connection-alist))
         (connection-product (nth 1 (nth 1 (assoc 'sql-product connection-info))))
         (sql-password (nth 1 (assoc connection tmtxt-sql-password))))
    ;; delete the connection info from the sql-connection-alist
    (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
    ;; delete the old password from the connection-info
    (setq connection-info (assq-delete-all 'sql-password connection-info))
    ;; add the password to the connection-info
    (nconc connection-info `((sql-password ,sql-password)))
    ;; add back the connection info to the beginning of sql-connection-alist
    ;; (last used server will appear first for the next prompt)
    (add-to-list 'sql-connection-alist connection-info)
    ;; override the sql-product by the product of this connection
    (setq sql-product connection-product)
    ;; connect
    (if current-prefix-arg
        (sql-connect connection connection)
      (sql-connect connection))))

(add-hook 'sql-mode-hook
          (lambda ()
            (setq-local ac-ignore-case t)
            (auto-complete-mode)))

(provide 'tmtxt-sql)
