;;; config for managing project

(require 'projectile)

;;; enable
(projectile-global-mode)

;;; config
(setq-default
 projectile-enable-caching t)

;;; ----
(defun find-csproj-containing-folders-recursively (directory)
  "Find all folders containing .csproj files in DIRECTORY recursively."
  (let ((result '()))
    (dolist (file (directory-files-recursively directory "\\.sln$"))
      (let ((folder (file-name-directory file)))
        (unless (member folder result)
          (setq result (append result (list folder))))))
    result))

;; (dolist (d (find-csproj-containing-folders-recursively "c:/git/GitHub/WiseTechGlobal/CargoWise.Customs/"))
;;   (projectile-add-known-project d)
;;   )

(provide 'tmtxt-project)
